%%%-------------------------------------------------------------------
%%% @doc
%%% role
%%% @end
%%%-------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([query/1, push/1]).
-export([online_time/1]).
-export([login/2, logout/2, disconnect/2, reconnect/2]).
-export([level/1, classes/1, sex/1]).
-export([upgrade_level/2, change_classes/2]).
-export([guild_id/1, guild_name/1, guild_job/1, guild_wealth/1]).
%% Includes
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("attribute.hrl").
-include("guild.hrl").
-include("asset.hrl").
-include("map.hrl").
-include("rank.hrl").
-include("role.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Role] = role_sql:select(RoleId),
    EventList = [
        #trigger{name = event_login, module = ?MODULE, function = login},
        #trigger{name = event_logout, module = ?MODULE, function = logout},
        #trigger{name = event_reconnect, module = ?MODULE, function = reconnect},
        #trigger{name = event_disconnect, module = ?MODULE, function = disconnect},
        #trigger{name = event_exp_add, module = ?MODULE, function = upgrade_level}
    ],
    NewUser = user_event:add_trigger(User, EventList),
    NewUser#user{role = Role, total_attribute = #attribute{}}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role = Role}) ->
    role_sql:update(Role#role{online_time = time:now()}),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{role = Role}) ->
    {ok, Role}.

%% @doc push
-spec push(User :: #user{}) -> ok.
push(User = #user{role = Role}) ->
    user_sender:send(User, ?PROTOCOL_ROLE_QUERY, Role).

%% @doc online time
-spec online_time(User :: #user{}) -> non_neg_integer().
online_time(#user{role = #role{online_time = OnlineTime}}) ->
    OnlineTime.

%% @doc login (load data complete)
-spec login(User :: #user{}, #event{}) -> ok().
login(User, _) ->
    %% calculate all attribute on load complete
    NewUser = attribute:calculate(User),
    {ok, map_server:enter(NewUser)}.

%% @doc logout (hosting timeout)
-spec logout(User :: #user{}, #event{}) -> ok().
logout(User, _) ->
    {ok, map_server:leave(User)}.

%% @doc reconnect
-spec reconnect(User :: #user{}, #event{}) -> ok().
reconnect(User, _) ->
    {ok, map_server:enter(User)}.

%% @doc disconnect
-spec disconnect(User :: #user{}, #event{}) -> ok().
disconnect(User, _) ->
    {ok, map_server:leave(User)}.

%% @doc upgrade level after add exp
-spec upgrade_level(User :: #user{}, #event{}) -> ok().
upgrade_level(User = #user{role = Role = #role{role_id = RoleId, role_name = RoleName, level = OldLevel}, asset = #asset{exp = Exp}}, _) ->
    NewLevel = role_data:level(Exp),
    _ = OldLevel =/= NewLevel andalso rank_server:update(?RANK_TYPE_LEVEL, #rank{type = ?RANK_TYPE_LEVEL, key = RoleId, value = NewLevel, time = time:now(), name = RoleName}) == ok andalso notice:broadcast(User, [level_upgrade, NewLevel]) == ok,
    NewUser = user_event:trigger(User#user{role = Role#role{level = NewLevel}}, [#event{name = event_level_upgrade, target = NewLevel}]),
    {ok, NewUser}.

%% @doc change classes
-spec change_classes(User :: #user{}, #event{}) -> ok().
change_classes(User = #user{role = Role}, NewClasses) ->
    {ok, User#user{role = Role#role{classes = NewClasses}}}.

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
guild_id(#user{role_id = RoleId}) ->
    guild:role_guild_id(RoleId).

%% @doc guild name
-spec guild_name(User :: #user{}) -> binary().
guild_name(User) ->
    (guild:get_guild(guild_id(User)))#guild.guild_name.

%% @doc guild job
-spec guild_job(User :: #user{}) -> non_neg_integer().
guild_job(#user{role_id = RoleId}) ->
    (guild:get_role(RoleId))#guild_role.job.

%% @doc guild wealth
-spec guild_wealth(User :: #user{}) -> non_neg_integer().
guild_wealth(#user{role_id = RoleId}) ->
    (guild:get_role(RoleId))#guild_role.wealth.

%%%===================================================================
%%% Internal functions
%%%===================================================================
