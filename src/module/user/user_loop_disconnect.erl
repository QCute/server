%%%-------------------------------------------------------------------
%%% @doc
%%% user disconnect loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_disconnect).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc disconnect loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2], User).

%% @doc disconnect loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_disconnect(Position, User)).

%% @doc disconnect loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_disconnect(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_disconnect(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_disconnect(#user.role, User) ->
    role:disconnect(User);
do_disconnect(_, User) ->
    User.
