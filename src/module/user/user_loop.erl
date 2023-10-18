%%%-------------------------------------------------------------------
%%% @doc
%%% user loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop).
%% API
-export([loop/4]).
%% Includes
-include("user.hrl").
%% Macros
-define(END_POSITION, 25).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc loop
-spec loop(User :: #user{}, non_neg_integer(), non_neg_integer(), non_neg_integer()) -> #user{}.
loop(User, Tick, Before, Now) ->
    ResetUser = case time:is_cross_day(Before, 0, Now) of
        true ->
            %% reset data at morning 0 o'clock
            user_loop_reset:loop(User);
        false ->
            User
    end,
    CleanUser = case time:is_cross_day(Before, 5, Now) of
        true ->
            %% clean data at morning 5 o'clock
            user_loop_clean:loop(ResetUser);
        false ->
            ResetUser
    end,
    TwoTickUser = case Tick rem 2 == 0 of
        true ->
            %% 2 times remove expire time data
            user_loop_expire:loop(CleanUser);
        false ->
            CleanUser
    end,
    FourTickUser = case Tick rem 4 == 0 of
        true ->
            %% 4 times primary important data
            user_loop_save:loop_range(#user.role, #user.vip, TwoTickUser);
        false ->
            TwoTickUser
    end,
    SixTickUser = case Tick rem 6 == 0 of
        true ->
            %% 6 times save count/item data
            user_loop_save:loop_range(#user.count, #user.item, FourTickUser);
        false ->
            FourTickUser
    end,
    EightTickUser = case Tick rem 8 == 0 of
        true ->
            %% 8 times save secondary important data
            user_loop_save:loop_range(#user.task, #user.daily, SixTickUser);
        false ->
            SixTickUser
    end,
    case Tick rem 10 == 0 of
        true ->
            %% 10 times save secondary important data
            user_loop_save:loop_range(#user.sign, #user.role_id, EightTickUser);
        false ->
            EightTickUser
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
