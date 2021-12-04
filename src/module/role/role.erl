%%%-------------------------------------------------------------------
%%% @doc
%%% role
%%% @end
%%%-------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([query/1, push/1]).
-export([login/1, logout/1, disconnect/1, reconnect/1]).
-export([handle_event_exp_add/1, change_sex/2, change_classes/2, change_name/2]).
-export([set_type/2, set_status/2]).
-export([level/1, sex/1, classes/1, logout_time/1]).
-export([guild_id/1, guild_name/1, guild_job/1, guild_wealth/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("online.hrl").
-include("attribute.hrl").
-include("guild.hrl").
-include("map.hrl").
-include("role.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Role] = role_sql:select(RoleId),
    %% update login time
    NewRole = Role#role{login_time = time:now()},
    %% fetch guild digest
    GuildId = guild:role_guild_id(RoleId),
    #guild_role{guild_name = GuildName, job = GuildJob, wealth = GuildWealth} = guild:get_role(RoleId, GuildId),
    User#user{role = NewRole, total_attribute = #attribute{}, guild_id = GuildId, guild_name = GuildName, guild_job = GuildJob, guild_wealth = GuildWealth}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role = Role, sender_pid = undefined}) ->
    %% update logout time
    NewRole = Role#role{logout_time = time:now()},
    role_sql:update(NewRole),
    User#user{role = NewRole};
save(User = #user{role = Role}) ->
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

%% @doc login (load data complete)
-spec login(User :: #user{}) -> #user{}.
login(User) ->
    %% calculate all attribute on load complete
    NewUser = attribute:calculate(User),
    map_server:enter(NewUser).

%% @doc logout (save data complete)
-spec logout(User :: #user{}) -> #user{}.
logout(User = #user{role = #role{role_id = RoleId, ip = Ip, device_id = DeviceId, login_time = LoginTime, logout_time = LogoutTime}}) ->
    %% log login at logout
    log:login_log(RoleId, Ip, DeviceId, LoginTime, LogoutTime - LoginTime, LogoutTime, time:now()),
    map_server:leave(User).

%% @doc reconnect
-spec reconnect(User :: #user{}) -> #user{}.
reconnect(User) ->
    map_server:enter(User).

%% @doc disconnect
-spec disconnect(User :: #user{}) -> #user{}.
disconnect(User) ->
    map_server:leave(User).

%% @doc upgrade level after add exp
-spec handle_event_exp_add(User :: #user{}) -> #user{}.
handle_event_exp_add(User = #user{role = Role = #role{level = OldLevel}}) ->
    NewLevel = role_data:level(asset:exp(User)),
    NewUser = User#user{role = Role#role{level = NewLevel}},
    case OldLevel < NewLevel of
        true ->
            user_event:trigger(NewUser, #event{name = event_level_upgrade, target = NewLevel});
        false ->
            NewUser
    end.

%% @doc change sex
-spec change_sex(User :: #user{}, NewSex :: non_neg_integer()) -> ok() | error().
change_sex(User = #user{role = Role = #role{sex = Sex}}, NewSex) when Sex =/= NewSex ->
    case item:cost(User, parameter_data:get(change_sex_cost), change_sex) of
        {ok, CostUser} ->
            NewUser = CostUser#user{role = Role#role{sex = NewSex}},
            FinalUser = user_event:trigger(NewUser, #event{name = event_sex_change, target = NewSex}),
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
            FinalUser = user_event:trigger(NewUser, #event{name = event_classes_change, target = NewClasses}),
            {ok, FinalUser};
        _ ->
            {error, item_not_enough}
    end;
change_classes(_, _) ->
    {error, role_cannot_change_same_classes}.

%% @doc change name
-spec change_name(User :: #user{}, NewName :: binary()) -> ok() | error().
change_name(User = #user{role = Role = #role{role_id = RoleId, role_name = RoleName}}, NewName) when RoleName =/= NewName ->
    case item:cost(User, parameter_data:get(change_name_cost), change_name) of
        {ok, CostUser} ->
            NewUser = CostUser#user{role_name = NewName, role = Role#role{role_name = NewName}},
            FinalUser = user_event:trigger(NewUser, #event{name = event_name_change, target = NewName}),
            %% update directly
            role_sql:update_name(NewName, RoleId),
            {ok, FinalUser};
        _ ->
            {error, item_not_enough}
    end;
change_name(_, _) ->
    {error, role_cannot_change_same_name}.

%% @doc set role type
-spec set_type(User :: #user{}, NewType :: non_neg_integer()) -> ok().
set_type(User = #user{role = Role}, Type = ?SERVER_STATE_FORBIDDEN) ->
    user_server:cast(self(), {stop, logout}),
    {ok, User#user{role = Role#role{type = Type}}};
set_type(User = #user{role = Role}, Type) ->
    {ok, User#user{role = Role#role{type = Type}}}.

%% @doc set role status
-spec set_status(User :: #user{}, Status :: non_neg_integer()) -> ok().
set_status(User = #user{role = Role}, Status = ?CHAT_STATE_UNLIMITED) ->
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
guild_id(#user{guild_id = GuildId}) ->
    GuildId.

%% @doc guild name
-spec guild_name(User :: #user{}) -> binary().
guild_name(#user{guild_name = GuildName}) ->
    GuildName.

%% @doc guild job
-spec guild_job(User :: #user{}) -> non_neg_integer().
guild_job(#user{guild_job = GuildJob}) ->
    GuildJob.

%% @doc guild wealth
-spec guild_wealth(User :: #user{}) -> non_neg_integer().
guild_wealth(#user{guild_wealth = GuildWealth}) ->
    GuildWealth.

%%%===================================================================
%%% Internal functions
%%%===================================================================
