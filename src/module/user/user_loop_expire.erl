%%%-------------------------------------------------------------------
%%% @doc
%%% user expire loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_expire).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc expire loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([6, 16, 19, 21, 22, 23], User).

%% @doc expire loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_expire(Position, User)).

%% @doc expire loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_expire(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_expire(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_expire(#user.item, User) ->
    item:expire(User);
do_expire(#user.mail, User) ->
    mail:expire(User);
do_expire(#user.buff, User) ->
    buff:expire(User);
do_expire(#user.fashion, User) ->
    fashion:expire(User);
do_expire(#user.title, User) ->
    title:expire(User);
do_expire(#user.bubble, User) ->
    bubble:expire(User);
do_expire(_, User) ->
    User.
