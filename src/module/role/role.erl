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
-export([check_quest/2]).
%% Includes
-include("user.hrl").
-include("event.hrl").
-include("role.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Role] = tool:default(role_sql:select(RoleId), [#role{}]),
    User#user{role = Role}.

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
-spec online_time(User :: #user{}) -> ok().
online_time(#user{role = #role{online_time = OnlineTime}}) ->
    OnlineTime.

%% @doc check quest
-spec check_quest(User :: #user{}, atom()) -> ok().
check_quest(#user{role = #role{level = Level}}, event_level_upgrade) ->
    #event_checker{data = Level};
check_quest(#user{role_id = RoleId}, event_guild_join) ->
    #event_checker{data = guild:role_guild_id(RoleId)}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
