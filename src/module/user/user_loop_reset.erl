%%%-------------------------------------------------------------------
%%% @doc
%%% user reset loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_reset).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc reset loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([5, 12, 14, 15, 24], User).

%% @doc reset loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_reset(Position, User)).

%% @doc reset loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_reset(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_reset(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_reset(#user.count, User) ->
    count:reset(User);
do_reset(#user.daily, User) ->
    daily:reset(User);
do_reset(#user.sign, User) ->
    sign:reset(User);
do_reset(#user.shop, User) ->
    shop:reset(User);
do_reset(#user.dungeon, User) ->
    dungeon:reset(User);
do_reset(_, User) ->
    User.
