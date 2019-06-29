%%%-------------------------------------------------------------------
%%% @doc
%%% module role
%%% @end
%%%-------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([clear/3]).
-export([save_timed_first/1, save_timed_second/1]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("assets.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
load(User = #user{id = Id}) ->
    Data = role_sql:select(Id),
    F = fun(Role = #role{focus = Focus}) -> Role#role{focus = parser:string_to_term(Focus)} end,
    [Role] = parser:convert(Data, role, F),
    User#user{role = Role}.

%% @doc save data
save(User = #user{role = Role}) ->
    role_sql:update(Role),
    User.

%% @doc save data timed
save_timed_first(User) ->
    role_logout:save_loop(#user.role, #user.assets, User).

%% @doc save data timed
save_timed_second(User) ->
    role_logout:save_loop(#user.quest, #user.shop, User).

%% @doc daily clear
clear(User, login, 0) ->
    User;
clear(User, login, 5) ->
    User;
clear(User, cross, 0) ->
    User;
clear(User, cross, 5) ->
    User.
%%%===================================================================
%%% Internal functions
%%%===================================================================