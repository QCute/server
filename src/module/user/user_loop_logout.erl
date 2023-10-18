%%%-------------------------------------------------------------------
%%% @doc
%%% user logout loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_logout).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc logout loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2], User).

%% @doc logout loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_logout(Position, User)).

%% @doc logout loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_logout(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_logout(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_logout(#user.role, User) ->
    role:logout(User);
do_logout(_, User) ->
    User.
