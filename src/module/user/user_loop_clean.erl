%%%-------------------------------------------------------------------
%%% @doc
%%% user clean loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_clean).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc clean loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([], User).

%% @doc clean loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_clean(Position, User)).

%% @doc clean loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_clean(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_clean(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_clean(_, User) ->
    User.
