%%%-------------------------------------------------------------------
%%% @doc
%%% role
%%% @end
%%%-------------------------------------------------------------------
-module(role).
%% API
-export([on_load/1, on_save/1]).
-export([query/1, push/1]).
-export([on_login/1, on_logout/1, on_disconnect/1, on_reconnect/1]).
-export([on_exp_add/1, change_sex/2, change_classes/2, change_name/2]).
-export([set_type/2, set_status/2]).
-export([level/1, sex/1, classes/1, logout_time/1]).
-export([guild_id/1, guild_name/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("online.hrl").
-include("attribute.hrl").
-include("guild.hrl").
-include("asset.hrl").
-include("device.hrl").
-include("role.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    [Role] = role_sql:select(RoleId),
    %% update login time
    NewRole = Role#role{login_time = time:now()},
    %% fetch guild digest
    GuildId = guild:role_guild_id(RoleId),
    User#user{role = NewRole, total_attribute = #attribute{}, guild = guild:get_role(RoleId, GuildId)}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{role = Role, sender_pid = undefined}) ->
    %% update logout time
    NewRole = Role#role{logout_time = time:now()},
    role_sql:update(NewRole),
    User#user{role = NewRole};
on_save(User = #user{role = Role}) ->
    role_sql:update(Role),
    User#user{role = Role}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{role = Role}) ->
    {ok, Role}.

%% @doc push
-spec push(User :: #user{}) -> ok.
push(User = #user{role = Role}) ->
    user_sender:send(User, ?PROTOCOL_ROLE_QUERY, Role).

%% @doc on login (load data complete)
-spec on_login(User :: #user{}) -> #user{}.
on_login(User = #user{role = #role{role_id = RoleId, login_time = LoginTime, logout_time = LogoutTime}, device = #device{ip = Ip, device_id = DeviceId}}) ->
    %% calculate all attribute on load complete
    %% log login at logout
    log:login_log(RoleId, Ip, DeviceId, LoginTime, max(0, LogoutTime - LoginTime), LogoutTime, time:now()),
    NewUser = attribute:calculate(User),
    map_server:enter(NewUser).

%% @doc on logout (save data complete)
-spec on_logout(User :: #user{}) -> #user{}.
on_logout(User = #user{role = #role{role_id = RoleId, login_time = LoginTime, logout_time = LogoutTime}, device = #device{ip = Ip, device_id = DeviceId}}) ->
    %% log login at logout
    log:login_log(RoleId, Ip, DeviceId, LoginTime, max(0, LogoutTime - LoginTime), LogoutTime, time:now()),
    map_server:leave(User).

%% @doc reconnect
-spec on_reconnect(User :: #user{}) -> #user{}.
on_reconnect(User) ->
    map_server:enter(User).

%% @doc on disconnect
-spec on_disconnect(User :: #user{}) -> #user{}.
on_disconnect(User) ->
    map_server:leave(User).

%% @doc upgrade level after add exp
-spec on_exp_add(User :: #user{}) -> #user{}.
on_exp_add(User = #user{role = Role = #role{level = OldLevel}, asset = #asset{exp = Exp}}) ->
    NewLevel = level_data:level(Exp),
    NewUser = User#user{role = Role#role{level = NewLevel}},
    case OldLevel < NewLevel of
        true ->
            user_event:trigger(NewUser, #event{name = level_upgrade, target = NewLevel});
        false ->
            NewUser
    end.

%% @doc change sex
-spec change_sex(User :: #user{}, NewSex :: non_neg_integer()) -> ok() | error().
change_sex(User = #user{role = Role = #role{sex = Sex}}, NewSex) when Sex =/= NewSex ->
    case item:cost(User, parameter_data:get(change_sex_cost), change_sex) of
        {ok, CostUser} ->
            NewUser = CostUser#user{role = Role#role{sex = NewSex}},
            FinalUser = user_event:trigger(NewUser, #event{name = sex_change, target = NewSex}),
            {ok, FinalUser};
        _ ->
            {error, item_not_enough}
    end;
change_sex(_, _) ->
    {error, role_cannot_change_same_sex}.

%% @doc change classes
-spec change_classes(User :: #user{}, NewClasses :: non_neg_integer()) -> ok() | error().
change_classes(User = #user{role = Role = #role{classes = Classes}}, NewClasses) when Classes =/= NewClasses ->
    case item:cost(User, parameter_data:get(change_classes_cost), change_classes) of
        {ok, CostUser} ->
            NewUser = CostUser#user{role = Role#role{classes = NewClasses}},
            FinalUser = user_event:trigger(NewUser, #event{name = classes_change, target = NewClasses}),
            {ok, FinalUser};
        _ ->
            {error, item_not_enough}
    end;
change_classes(_, _) ->
    {error, role_cannot_change_same_classes}.

%% @doc change name
-spec change_name(User :: #user{}, NewName :: binary()) -> ok() | error().
change_name(User = #user{role = Role = #role{role_name = RoleName}}, NewName) when RoleName =/= NewName ->
    case item:cost(User, parameter_data:get(change_name_cost), change_name) of
        {ok, CostUser} ->
            NewRole = Role#role{role_name = NewName},
            NewUser = CostUser#user{role_name = NewName, role = NewRole},
            FinalUser = user_event:trigger(NewUser, #event{name = name_change, target = NewName}),
            %% update directly
            role_sql:update_name(NewRole),
            {ok, FinalUser};
        _ ->
            {error, item_not_enough}
    end;
change_name(_, _) ->
    {error, role_cannot_change_same_name}.

%% @doc set role type
-spec set_type(User :: #user{}, NewType :: non_neg_integer()) -> ok().
set_type(User = #user{role = Role}, Type = ?SERVER_STATE_BAN) ->
    user_server:cast(self(), {stop, logout}),
    {ok, User#user{role = Role#role{type = Type}}};
set_type(User = #user{role = Role}, Type) ->
    {ok, User#user{role = Role#role{type = Type}}}.

%% @doc set role status
-spec set_status(User :: #user{}, Status :: non_neg_integer()) -> ok().
set_status(User = #user{role = Role}, Status = ?CHAT_STATE_NORMAL) ->
    {ok, User#user{role = Role#role{status = Status}}};
set_status(User = #user{role = Role = #role{status = OldStatus}}, Status) ->
    {ok, User#user{role = Role#role{status = OldStatus bor Status}}}.

%% @doc level
-spec level(User :: #user{}) -> non_neg_integer().
level(#user{role = #role{level = Level}}) ->
    Level.

%% @doc sex
-spec sex(User :: #user{}) -> non_neg_integer().
sex(#user{role = #role{sex = Sex}}) ->
    Sex.

%% @doc classes
-spec classes(User :: #user{}) -> non_neg_integer().
classes(#user{role = #role{classes = Classes}}) ->
    Classes.

%% @doc logout time
-spec logout_time(User :: #user{}) -> non_neg_integer().
logout_time(#user{role = #role{logout_time = LogoutTime}}) ->
    LogoutTime.

%% @doc guild id
-spec guild_id(User :: #user{}) -> non_neg_integer().
guild_id(#user{guild = #guild_role{guild_id = GuildId}}) ->
    GuildId.

%% @doc guild name
-spec guild_name(User :: #user{}) -> binary().
guild_name(#user{guild = #guild_role{guild_name = GuildName}}) ->
    GuildName.

%%%===================================================================
%%% Internal functions
%%%===================================================================
