%%%-------------------------------------------------------------------
%%% @doc
%%% module user data saver
%%% @end
%%%-------------------------------------------------------------------
-module(user_saver).
%% API
-export([save/1]).
-export([save_loop/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc save data on role logout
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User) ->
    Size = tuple_size(User),
    save_loop(2, Size, User).

%% @doc save loop
-spec save_loop(Position :: pos_integer(), Size :: pos_integer(), User :: #user{}) -> NewUser :: #user{}.
save_loop(Size, Size, User) ->
    do_save(Size, User);
save_loop(Position, Size, User) ->
    NewUser = do_save(Position, User),
    save_loop(Position + 1, Size, NewUser).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% save per role's data
do_save(#user.role, User) ->
    role:save(User);
do_save(#user.asset, User) ->
    asset:save(User);
do_save(#user.vip, User) ->
    vip:save(User);
do_save(#user.item, User) ->
    item:save(User);
do_save(#user.quest, User) ->
    quest:save(User);
do_save(#user.friend, User) ->
    friend:save(User);
do_save(#user.shop, User) ->
    shop:save(User);
do_save(#user.buff, User) ->
    buff:save(User);
do_save(#user.skill, User) ->
    skill:save(User);
do_save(#user.count, User) ->
    count:save(User);
do_save(_, User) ->
    User.
