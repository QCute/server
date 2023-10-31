%%%-------------------------------------------------------------------
%%% @doc
%%% user create loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_create).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc create loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2], User).

%% @doc create loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_create(Position, User)).

%% @doc create loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_create(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_create(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_create(#user.role, User) ->
    role:create(User);
do_create(_, User) ->
    User.
