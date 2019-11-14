%%%------------------------------------------------------------------
%%% @doc
%%% module role
%%% @end
%%%------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([query/1]).
%% Includes
-include("user.hrl").
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
    role_sql:update(Role),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{role = Role}) ->
    {ok, [Role]}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
