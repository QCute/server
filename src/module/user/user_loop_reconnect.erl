%%%-------------------------------------------------------------------
%%% @doc
%%% user reconnect loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_reconnect).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc reconnect loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2], User).

%% @doc reconnect loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_reconnect(Position, User)).

%% @doc reconnect loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_reconnect(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_reconnect(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_reconnect(#user.role, User) ->
    role:reconnect(User);
do_reconnect(_, User) ->
    User.
