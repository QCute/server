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
            event:trigger(User, #event{name = reset});
        false ->
            User
    end,
    CleanUser = case time:is_cross_day(Before, 5, Now) of
        true ->
            %% clean data at morning 5 o'clock
            event:trigger(User, #event{name = clean});
        false ->
            ResetUser
    end,
    TwoTickUser = case Tick rem 2 == 0 of
        true ->
            %% 2 times remove expire time data
            event:trigger(User, #event{name = expire});
        false ->
            CleanUser
    end,
    FourTickUser = case Tick rem 4 == 0 of
        true ->
            %% 4 times primary important data
            event:trigger(User, #event{name = save, target = 4});
        false ->
            TwoTickUser
    end,
    SixTickUser = case Tick rem 6 == 0 of
        true ->
            %% 6 times save count/item data
            event:trigger(User, #event{name = save, target = 6});
        false ->
            FourTickUser
    end,
    EightTickUser = case Tick rem 8 == 0 of
        true ->
            %% 8 times save secondary important data
            event:trigger(User, #event{name = save, target = 8});
        false ->
            SixTickUser
    end,
    case Tick rem 10 == 0 of
        true ->
            %% 10 times save secondary important data
            event:trigger(User, #event{name = save, target = 10});
        false ->
            EightTickUser
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
