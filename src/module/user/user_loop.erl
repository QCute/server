%%%-------------------------------------------------------------------
%%% @doc
%%% user loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop).
%% API
-export([loop/4]).
-export([load/1, load_loop/2, load_loop/3]).
-export([save/1, save_loop/2, save_loop/3]).
-export([reset/1, reset_loop/2, reset_loop/3]).
-export([clean/1, clean_loop/2, clean_loop/3]).
-export([expire/1, expire_loop/2, expire_loop/3]).
-export([login/1, login_loop/2, login_loop/3]).
-export([logout/1, logout_loop/2, logout_loop/3]).
-export([reconnect/1, reconnect_loop/2, reconnect_loop/3]).
-export([disconnect/1, disconnect_loop/2, disconnect_loop/3]).
%% Includes
-include("user.hrl").
%% Macros
-define(END_POSITION, 25).
-define(LOAD_LIST, [2, 3, 4, 5, 6, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]).
-define(SAVE_LIST, [2, 3, 4, 5, 6, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]).
-define(RESET_LIST, [5, 12, 14, 15, 24]).
-define(CLEAN_LIST, []).
-define(EXPIRE_LIST, [6, 16, 19, 21, 22, 23]).
-define(LOGIN_LIST, [2]).
-define(LOGOUT_LIST, [2]).
-define(RECONNECT_LIST, [2]).
-define(DISCONNECT_LIST, [2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc loop
-spec loop(User :: #user{}, non_neg_integer(), non_neg_integer(), non_neg_integer()) -> #user{}.
loop(User, Tick, Before, Now) ->
    ResetUser = case time:is_cross_day(Before, 0, Now) of
        true ->
            %% reset data at morning 0 o'clock
            reset(User);
        false ->
            User
    end,
    CleanUser = case time:is_cross_day(Before, 5, Now) of
        true ->
            %% clean data at morning 5 o'clock
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
            %% 4 times primary important data
            save_loop(#user.role, #user.vip, TwoTickUser);
        false ->
            TwoTickUser
    end,
    SixTickUser = case Tick rem 6 == 0 of
        true ->
            %% 6 times save count/item data
            save_loop(#user.count, #user.item, FourTickUser);
        false ->
            FourTickUser
    end,
    EightTickUser = case Tick rem 8 == 0 of
        true ->
            %% 8 times save secondary important data
            save_loop(#user.task, #user.daily, SixTickUser);
        false ->
            SixTickUser
    end,
    case Tick rem 10 == 0 of
        true ->
            %% 10 times save secondary important data
            save_loop(#user.sign, #user.role_id, EightTickUser);
        false ->
            EightTickUser
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

%% @doc login after loaded
-spec login(User :: #user{}) -> NewUser :: #user{}.
login(User) ->
    login_loop(?LOGIN_LIST, User).

%% @doc login loop
-spec login_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
login_loop([], User) ->
    User;
login_loop([Position | T], User) ->
    login_loop(T, do_login(Position, User)).

%% @doc login loop
-spec login_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
login_loop(Size, Size, User) ->
    do_login(Size, User);
login_loop(Position, Size, User) ->
    login_loop(Position + 1, Size, do_login(Position, User)).

%% @doc logout after saved
-spec logout(User :: #user{}) -> NewUser :: #user{}.
logout(User) ->
    logout_loop(?LOGOUT_LIST, User).

%% @doc logout loop
-spec logout_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
logout_loop([], User) ->
    User;
logout_loop([Position | T], User) ->
    logout_loop(T, do_logout(Position, User)).

%% @doc logout loop
-spec logout_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
logout_loop(Size, Size, User) ->
    do_logout(Size, User);
logout_loop(Position, Size, User) ->
    logout_loop(Position + 1, Size, do_logout(Position, User)).

%% @doc reconnect
-spec reconnect(User :: #user{}) -> NewUser :: #user{}.
reconnect(User) ->
    reconnect_loop(?RECONNECT_LIST, User).

%% @doc reconnect loop
-spec reconnect_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
reconnect_loop([], User) ->
    User;
reconnect_loop([Position | T], User) ->
    reconnect_loop(T, do_reconnect(Position, User)).

%% @doc reconnect loop
-spec reconnect_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
reconnect_loop(Size, Size, User) ->
    do_reconnect(Size, User);
reconnect_loop(Position, Size, User) ->
    reconnect_loop(Position + 1, Size, do_reconnect(Position, User)).

%% @doc disconnect
-spec disconnect(User :: #user{}) -> NewUser :: #user{}.
disconnect(User) ->
    disconnect_loop(?DISCONNECT_LIST, User).

%% @doc disconnect loop
-spec disconnect_loop(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
disconnect_loop([], User) ->
    User;
disconnect_loop([Position | T], User) ->
    disconnect_loop(T, do_disconnect(Position, User)).

%% @doc disconnect loop
-spec disconnect_loop(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
disconnect_loop(Size, Size, User) ->
    do_disconnect(Size, User);
disconnect_loop(Position, Size, User) ->
    disconnect_loop(Position + 1, Size, do_disconnect(Position, User)).

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

do_clean(_, User) ->
    User.

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

do_login(#user.role, User) ->
    role:login(User);
do_login(_, User) ->
    User.

do_logout(#user.role, User) ->
    role:logout(User);
do_logout(_, User) ->
    User.

do_reconnect(#user.role, User) ->
    role:reconnect(User);
do_reconnect(_, User) ->
    User.

do_disconnect(#user.role, User) ->
    role:disconnect(User);
do_disconnect(_, User) ->
    User.

