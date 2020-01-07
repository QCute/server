%%%------------------------------------------------------------------
%%% @doc
%%% module user loop
%%% @end
%%%------------------------------------------------------------------
-module(user_loop).
%% API
-export([loop/3]).
-export([load/1, load_loop/2, load_loop/3]).
-export([save/1, save_loop/2, save_loop/3]).
-export([reset/1, reset_loop/2, reset_loop/3]).
-export([clean/1, clean_loop/2, clean_loop/3]).
-export([expire/1, expire_loop/2, expire_loop/3]).
%% Includes
-include("user.hrl").
%% Macros
-define(END_POSITION,15).
-define(LOAD_LIST,[2,3,4,5,9,10,11,12,13,14,15]).
-define(SAVE_LIST,[2,3,4,5,9,11,12,13,14,15]).
-define(RESET_LIST,[12,15]).
-define(CLEAN_LIST,[]).
-define(EXPIRE_LIST,[5,13]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc loop
-spec loop(User :: #user{}, non_neg_integer(), non_neg_integer()) -> #user{}.
loop(User = #user{tick = Tick}, Last, Now) ->
    Now = time:ts(),
    ResetUser = case time:cross(day, 0, Last, Now) of
        true ->
            %% reset data at morning 0 hour
            reset(User);
        false ->
            User
    end,
    CleanUser = case time:cross(day, 5, Last, Now) of
        true ->
            %% clean data at morning 5 hour
            clean(ResetUser);
        false ->
            ResetUser
    end,
    TwoTickUser = case Tick rem 2 == 0 of
        true ->
            %% 2 times remove expire time data
            user_loop:expire(CleanUser);
        false ->
            CleanUser
    end,
    FourTickUser = case Tick rem 4 == 0 of
        true ->
            %% 4 times save important data
            save_loop(#user.role, #user.vip, TwoTickUser);
        false ->
            TwoTickUser
    end,
    case Tick rem 6 == 0 of
        true ->
            %% 6 times save secondary important data
            save_loop(#user.item, #user.count, FourTickUser);
        false ->
            FourTickUser
    end.

%% @doc load data
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User) ->
    load_loop(?LOAD_LIST, User).

%% @doc load loop
-spec load_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
load_loop([], User) ->
    User;
load_loop([Position | T], User) ->
    load_loop(T, do_load(Position, User)).

%% @doc load loop
-spec load_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
load_loop(Size, Size, User) ->
    do_load(Size, User);
load_loop(Position, Size, User) ->
    load_loop(Position + 1, Size, do_load(Position, User)).

%% @doc save data
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User) ->
    save_loop(?SAVE_LIST, User).

%% @doc save loop
-spec save_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
save_loop([], User) ->
    User;
save_loop([Position | T], User) ->
    save_loop(T, do_save(Position, User)).

%% @doc save loop
-spec save_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
save_loop(Size, Size, User) ->
    do_save(Size, User);
save_loop(Position, Size, User) ->
    save_loop(Position + 1, Size, do_save(Position, User)).

%% @doc reset data
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User) ->
    reset_loop(?RESET_LIST, User).

%% @doc reset loop
-spec reset_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
reset_loop([], User) ->
    User;
reset_loop([Position | T], User) ->
    reset_loop(T, do_reset(Position, User)).

%% @doc reset loop
-spec reset_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
reset_loop(Size, Size, User) ->
    do_reset(Size, User);
reset_loop(Position, Size, User) ->
    reset_loop(Position + 1, Size, do_reset(Position, User)).

%% @doc clean data
-spec clean(User :: #user{}) -> NewUser :: #user{}.
clean(User) ->
    clean_loop(?CLEAN_LIST, User).

%% @doc clean loop
-spec clean_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
clean_loop([], User) ->
    User;
clean_loop([Position | T], User) ->
    clean_loop(T, do_clean(Position, User)).

%% @doc clean loop
-spec clean_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
clean_loop(Size, Size, User) ->
    do_clean(Size, User);
clean_loop(Position, Size, User) ->
    clean_loop(Position + 1, Size, do_clean(Position, User)).

%% @doc remove expire data
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User) ->
    expire_loop(?EXPIRE_LIST, User).

%% @doc expire loop
-spec expire_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
expire_loop([], User) ->
    User;
expire_loop([Position | T], User) ->
    expire_loop(T, do_expire(Position, User)).

%% @doc expire loop
-spec expire_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
expire_loop(Size, Size, User) ->
    do_expire(Size, User);
expire_loop(Position, Size, User) ->
    expire_loop(Position + 1, Size, do_expire(Position, User)).

%%%==================================================================
%%% Internal functions
%%%==================================================================
do_load(#user.role, User) ->
    role:load(User);
do_load(#user.asset, User) ->
    asset:load(User);
do_load(#user.vip, User) ->
    vip:load(User);
do_load(#user.item, User) ->
    item:load(User);
do_load(#user.quest, User) ->
    quest:load(User);
do_load(#user.mail, User) ->
    mail:load(User);
do_load(#user.friend, User) ->
    friend:load(User);
do_load(#user.shop, User) ->
    shop:load(User);
do_load(#user.buff, User) ->
    buff:load(User);
do_load(#user.skill, User) ->
    skill:load(User);
do_load(#user.count, User) ->
    count:load(User);
do_load(_, User) ->
    User.

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

do_reset(#user.shop, User) ->
    shop:reset(User);
do_reset(#user.count, User) ->
    count:reset(User);
do_reset(_, User) ->
    User.

do_clean(_, User) ->
    User.

do_expire(#user.item, User) ->
    item:expire(User);
do_expire(#user.buff, User) ->
    buff:expire(User);
do_expire(_, User) ->
    User.

