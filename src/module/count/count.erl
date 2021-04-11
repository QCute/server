%%%-------------------------------------------------------------------
%%% @doc
%%% count
%%% @end
%%%-------------------------------------------------------------------
-module(count).
%% API
-export([load/1, save/1, reset/1]).
-export([handle_event_recharge/2, handle_event_gold_cost/2, handle_event_shop_buy/2]).
-export([add/2, add/3]).
-export([add_today/2, add_today/3]).
-export([add_week/2, add_week/3]).
-export([add_total/2, add_total/3]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("recharge.hrl").
-include("count.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Count = count_sql:select_by_role_id(RoleId),
    User#user{count = Count}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{count = Count}) ->
    NewCount = count_sql:insert_update(Count),
    User#user{count = NewCount}.

%% @doc reset
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{count = CountList}) ->
    case time:weekday() of
        1 ->
            NewCountList = [Count#count{today_number = 0, week_number = 0, flag = 1} || Count <- CountList];
        _ ->
            NewCountList = [Count#count{today_number = 0, flag = 1} || Count <- CountList]
    end,
    User#user{count = NewCountList}.

%% @doc increase recharge count after recharge
-spec handle_event_recharge(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
handle_event_recharge(User, #event{name = event_recharge}) ->
    add(User, ?COUNT_TYPE_RECHARGE).

%% @doc increase gold cost count after cost gold
-spec handle_event_gold_cost(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
handle_event_gold_cost(User, #event{name = event_gold_cost, number = Number}) ->
    add(User, ?COUNT_TYPE_COST_GOLD, Number).

%% @doc increase shop buy count after buy shop
-spec handle_event_shop_buy(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
handle_event_shop_buy(User, #event{name = event_shop_buy, number = Number}) ->
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
    User#user{count = NewCountList}.

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
    User#user{count = NewCountList}.

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
    User#user{count = NewCountList}.

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
    User#user{count = NewCountList}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
