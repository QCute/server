%%%-------------------------------------------------------------------
%%% @doc
%%% user load loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_load).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2, 3, 4, 5, 6, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24], User).

%% @doc load loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_load(Position, User)).

%% @doc load loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_load(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_load(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_load(#user.role, User) ->
    role:load(User);
do_load(#user.asset, User) ->
    asset:load(User);
do_load(#user.vip, User) ->
    vip:load(User);
do_load(#user.count, User) ->
    count:load(User);
do_load(#user.item, User) ->
    item:load(User);
do_load(#user.task, User) ->
    task:load(User);
do_load(#user.achievement, User) ->
    achievement:load(User);
do_load(#user.daily, User) ->
    daily:load(User);
do_load(#user.sign, User) ->
    sign:load(User);
do_load(#user.shop, User) ->
    shop:load(User);
do_load(#user.mail, User) ->
    mail:load(User);
do_load(#user.notice, User) ->
    notice:load(User);
do_load(#user.friend, User) ->
    friend:load(User);
do_load(#user.buff, User) ->
    buff:load(User);
do_load(#user.skill, User) ->
    skill:load(User);
do_load(#user.fashion, User) ->
    fashion:load(User);
do_load(#user.title, User) ->
    title:load(User);
do_load(#user.bubble, User) ->
    bubble:load(User);
do_load(#user.dungeon, User) ->
    dungeon:load(User);
do_load(_, User) ->
    User.
