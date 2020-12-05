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
-export([level/1, classes/1, sex/1]).
-export([upgrade_level/1, change_classes/2]).
-export([guild_id/1, guild_name/1, guild_job/1, guild_wealth/1]).
%% Includes
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
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
save(User = #user{role = Role}) ->
    NewRole = Role#role{logout_time = time:now()},
    role_sql:update(NewRole),
    User#user{role = NewRole}.

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
logout(User) ->
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
-spec upgrade_level(User :: #user{}) -> #user{}.
upgrade_level(User = #user{role = Role = #role{level = OldLevel}}) ->
    NewLevel = role_data:level(asset:exp(User)),
    NewUser = User#user{role = Role#role{level = NewLevel}},
    case OldLevel < NewLevel of
        true ->
            user_event:trigger(NewUser, #event{name = event_level_upgrade, target = NewLevel});
        false ->
            NewUser
    end.

%% @doc change classes
-spec change_classes(User :: #user{}, NewClasses :: non_neg_integer()) -> ok().
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
    {error, cannot_change_to_same_classes}.

%% @doc level
-spec level(User :: #user{}) -> non_neg_integer().
level(#user{role = #role{level = Level}}) ->
    Level.

%% @doc classes
-spec classes(User :: #user{}) -> non_neg_integer().
classes(#user{role = #role{classes = Classes}}) ->
    Classes.

%% @doc sex
-spec sex(User :: #user{}) -> non_neg_integer().
sex(#user{role = #role{sex = Sex}}) ->
    Sex.

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
