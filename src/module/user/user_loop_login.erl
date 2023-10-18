%%%-------------------------------------------------------------------
%%% @doc
%%% user login loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_login).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc login loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2], User).

%% @doc login loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_login(Position, User)).

%% @doc login loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_login(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_login(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_login(#user.role, User) ->
    role:login(User);
do_login(_, User) ->
    User.
