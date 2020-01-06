%%%------------------------------------------------------------------
%%% @doc
%%% module role
%%% @end
%%%------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([reset_clean/1]).
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
    [Role] = parser:convert(role_sql:select(RoleId), ?MODULE, fun(Role = #role{map = Map}) -> Role#role{map = parser:to_term(Map)} end),
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

%% @doc reset clean
-spec reset_clean(User :: #user{}) -> ok().
reset_clean(User = #user{role = #role{online_time = OnlineTime}}) ->
    ResetUser = case time:cross(day, 0, OnlineTime, time:ts()) of
        true ->
            user_loop:reset(User);
        false ->
            User
    end,
    case time:cross(day, 5, OnlineTime, time:ts()) of
        true ->
            user_loop:clean(ResetUser);
        false ->
            ResetUser
    end.

%% @doc check quest
-spec check_quest(User :: #user{}, atom()) -> ok().
check_quest(#user{role = #role{level = Level}}, event_level_upgrade) ->
    #event_checker{data = Level}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
