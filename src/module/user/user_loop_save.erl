%%%-------------------------------------------------------------------
%%% @doc
%%% user save loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_save).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc save loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([2, 3, 4, 5, 6, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24], User).

%% @doc save loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_save(Position, User)).

%% @doc save loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_save(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_save(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_save(#user.role, User) ->
    role:save(User);
do_save(#user.asset, User) ->
    asset:save(User);
do_save(#user.vip, User) ->
    vip:save(User);
do_save(#user.count, User) ->
    count:save(User);
do_save(#user.item, User) ->
    item:save(User);
do_save(#user.task, User) ->
    task:save(User);
do_save(#user.achievement, User) ->
    achievement:save(User);
do_save(#user.daily, User) ->
    daily:save(User);
do_save(#user.shop, User) ->
    shop:save(User);
do_save(#user.mail, User) ->
    mail:save(User);
do_save(#user.notice, User) ->
    notice:save(User);
do_save(#user.friend, User) ->
    friend:save(User);
do_save(#user.buff, User) ->
    buff:save(User);
do_save(#user.skill, User) ->
    skill:save(User);
do_save(#user.fashion, User) ->
    fashion:save(User);
do_save(#user.title, User) ->
    title:save(User);
do_save(#user.bubble, User) ->
    bubble:save(User);
do_save(#user.dungeon, User) ->
    dungeon:save(User);
do_save(_, User) ->
    User.
