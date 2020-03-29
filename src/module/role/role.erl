%%%------------------------------------------------------------------
%%% @doc
%%% module role
%%% @end
%%%------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([online_time/1]).
-export([login/2, logout/2, disconnect/2, reconnect/2]).
-export([check_quest/2]).
-export([upgrade_level/2]).
-export([guild_id/1, guild_name/1, guild_job/1, guild_wealth/1]).
%% Includes
-include("user.hrl").
-include("event.hrl").
-include("attribute.hrl").
-include("guild.hrl").
-include("asset.hrl").
-include("map.hrl").
-include("role.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Role] = role_sql:select(RoleId),
    EventList = [
        #trigger{name = login, module = ?MODULE, function = login},
        #trigger{name = logout, module = ?MODULE, function = logout},
        #trigger{name = reconnect, module = ?MODULE, function = reconnect},
        #trigger{name = disconnect, module = ?MODULE, function = disconnect},
        #trigger{name = add_exp, module = ?MODULE, function = upgrade_level}
    ],
    NewUser = user_event:add(User, EventList),
    NewUser#user{role = Role, total_attribute = #attribute{}}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role = Role}) ->
    role_sql:update(Role#role{online_time = time:ts()}),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{role = Role}) ->
    {ok, Role}.

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

%% @doc check quest
-spec check_quest(User :: #user{}, atom()) -> #event_checker{}.
check_quest(#user{role = #role{level = Level}}, event_level_upgrade) ->
    #event_checker{data = Level};
check_quest(#user{role_id = RoleId}, event_guild_join) ->
    #event_checker{data = guild_id(RoleId)}.

%% @doc upgrade level after add exp
-spec upgrade_level(User :: #user{}, #event{}) -> ok().
upgrade_level(User = #user{role = Role, asset = #asset{exp = Exp}}, _) ->
    Level = role_data:level(Exp),
    {ok, User#user{role = Role#role{level = Level}}}.

-spec guild_id(User :: #user{}) -> non_neg_integer().
guild_id(#user{role_id = RoleId}) ->
    guild:role_guild_id(RoleId).

-spec guild_name(User :: #user{}) -> binary().
guild_name(User) ->
    case guild:get_guild(guild_id(User)) of
        [#guild{guild_name = GuildName}] ->
            GuildName;
        _ ->
            <<>>
    end.

-spec guild_job(User :: #user{}) -> non_neg_integer().
guild_job(#user{role_id = RoleId}) ->
    case guild:get_role(RoleId) of
        [#guild_role{job = Job}] ->
            Job;
        _ ->
            0
    end.

-spec guild_wealth(User :: #user{}) -> non_neg_integer().
guild_wealth(#user{role_id = RoleId}) ->
    case guild:get_role(RoleId) of
        [#guild_role{wealth = Wealth}] ->
            Wealth;
        _ ->
            0
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
