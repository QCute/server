%%%-------------------------------------------------------------------
%%% @doc
%%% count
%%% @end
%%%-------------------------------------------------------------------
-module(count).
%% API
-export([on_load/1, on_save/1, on_reset/1]).
-export([on_charge/2, on_gold_cost/2, on_shop_buy/2]).
-export([add/2, add/3, get/2]).
-export([add_today/2, add_today/3, get_today/2]).
-export([add_week/2, add_week/3, get_week/2]).
-export([set/5, set_today/3, set_week/3, set_total/3]).
-export([add_total/2, add_total/3, get_total/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("count.hrl").
-include("event.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Count = count_sql:select(RoleId),
    User#user{count = Count}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{count = Count}) ->
    NewCount = count_sql:save(Count),
    User#user{count = NewCount}.

%% @doc on reset
-spec on_reset(User :: #user{}) -> NewUser :: #user{}.
on_reset(User = #user{count = CountList}) ->
    case time:weekday() of
        1 ->
            NewCountList = [Count#count{today_number = 0, week_number = 0, flag = 1} || Count <- CountList];
        _ ->
            NewCountList = [Count#count{today_number = 0, flag = 1} || Count <- CountList]
    end,
    User#user{count = NewCountList}.

%% @doc increase charge count after charge
-spec on_charge(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
on_charge(User, #event{name = charge}) ->
    add(User, ?COUNT_TYPE_CHARGE).

%% @doc increase gold cost count after cost gold
-spec on_gold_cost(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
on_gold_cost(User, #event{name = gold_cost, number = Number}) ->
    add(User, ?COUNT_TYPE_COST_GOLD, Number).

%% @doc increase shop buy count after buy shop
-spec on_shop_buy(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
on_shop_buy(User, #event{name = shop_buy, number = Number}) ->
    add(User, ?COUNT_TYPE_SHOP_BUY, Number).

%% @doc add
-spec add(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add(User#user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add
-spec add(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber, week_number = WeekNumber, total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, week_number = WeekNumber + Number, total_number = TotalNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    NewUser = User#user{count = NewCountList},
    user_event:trigger(NewUser, #event{name = count, target = Type, number = Number}).

%% @doc add today
-spec add_today(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add_today(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add_today(User#user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add today
-spec add_today(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_today(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    NewUser = User#user{count = NewCountList},
    user_event:trigger(NewUser, #event{name = count, target = Type, number = Number, extra = today}).

%% @doc add week
-spec add_week(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add_week(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add_week(User#user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add week
-spec add_week(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_week(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{week_number = WeekNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{week_number = WeekNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    NewUser = User#user{count = NewCountList},
    user_event:trigger(NewUser, #event{name = count, target = Type, number = Number, extra = week}).

%% @doc add total
-spec add_total(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add_total(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add_total(User = #user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add total
-spec add_total(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_total(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{total_number = TotalNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    NewUser = User#user{count = NewCountList},
    user_event:trigger(NewUser, #event{name = count, target = Type, number = Number, extra = total}).

%% @doc set
-spec set(User :: #user{}, Type :: non_neg_integer(), TodayNumber :: non_neg_integer(), WeekNumber :: non_neg_integer(), TotalNumber :: non_neg_integer()) -> NewUser :: #user{}.
set(User = #user{role_id = RoleId, count = CountList}, Type, TodayNumber, WeekNumber, TotalNumber) ->
    Count = #count{today_number = TodayNumber, week_number = WeekNumber, total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber, week_number = WeekNumber, total_number = TotalNumber, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc set today
-spec set_today(User :: #user{}, Type :: non_neg_integer(), TodayNumber :: non_neg_integer()) -> NewUser :: #user{}.
set_today(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc set week
-spec set_week(User :: #user{}, Type :: non_neg_integer(), WeekNumber :: non_neg_integer()) -> NewUser :: #user{}.
set_week(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{week_number = WeekNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{week_number = WeekNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc set total
-spec set_total(User :: #user{}, Type :: non_neg_integer(), TotalNumber :: non_neg_integer()) -> NewUser :: #user{}.
set_total(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{total_number = TotalNumber + Number, time = time:now(), flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc get
-spec get(User :: #user{}, Type :: non_neg_integer()) -> #count{}.
get(#user{role_id = RoleId, count = CountList}, Type) ->
    listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}).

%% @doc get today
-spec get_today(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
get_today(#user{role_id = RoleId, count = CountList}, Type) ->
    #count{today_number = TodayNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    TodayNumber.

%% @doc get week
-spec get_week(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
get_week(#user{role_id = RoleId, count = CountList}, Type) ->
    #count{week_number = WeekNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    WeekNumber.

%% @doc get total
-spec get_total(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
get_total(#user{role_id = RoleId, count = CountList}, Type) ->
    #count{total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    TotalNumber.

%%%===================================================================
%%% Internal functions
%%%===================================================================
