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
-include("event.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc loop
-spec loop(User :: #user{}, non_neg_integer(), non_neg_integer(), non_neg_integer()) -> #user{}.
loop(User, Tick, Before, Now) ->
    ResetUser = case time:is_cross_day(Before, 0, Now) of
        true ->
            %% reset data at morning 0 o'clock
            user_event:trigger(User, #event{name = reset});
        false ->
            User
    end,
    CleanUser = case time:is_cross_day(Before, 5, Now) of
        true ->
            %% clean data at morning 5 o'clock
            user_event:trigger(ResetUser, #event{name = clean});
        false ->
            ResetUser
    end,
    TwoTickUser = case Tick rem 2 == 0 of
        true ->
            %% 2 times remove expire time data
            user_event:trigger(CleanUser, #event{name = expire});
        false ->
            CleanUser
    end,
    FourTickUser = case Tick rem 4 == 0 of
        true ->
            %% 4 times primary important data
            user_event:trigger(TwoTickUser, #event{name = save, target = 4});
        false ->
            TwoTickUser
    end,
    SixTickUser = case Tick rem 6 == 0 of
        true ->
            %% 6 times save count/item data
            user_event:trigger(FourTickUser, #event{name = save, target = 6});
        false ->
            FourTickUser
    end,
    EightTickUser = case Tick rem 8 == 0 of
        true ->
            %% 8 times save secondary important data
            user_event:trigger(SixTickUser, #event{name = save, target = 8});
        false ->
            SixTickUser
    end,
    case Tick rem 10 == 0 of
        true ->
            %% 10 times save secondary important data
            user_event:trigger(EightTickUser, #event{name = save, target = 10});
        false ->
            EightTickUser
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
