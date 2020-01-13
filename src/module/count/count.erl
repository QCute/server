%%%------------------------------------------------------------------
%%% @doc
%%% module count
%%% @end
%%%------------------------------------------------------------------
-module(count).
%% API
-export([load/1, save/1, reset/1]).
-export([add/2, add/3]).
-export([add_today/2, add_today/3]).
-export([add_total/2, add_total/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("count.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Count = count_sql:select(RoleId),
    User#user{count = Count}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{count = Count}) ->
    NewCount = count_sql:insert_update(Count),
    User#user{count = NewCount}.

%% @doc clean
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{count = CountList}) ->
    NewCountList = [Count#count{today_number = 0, flag = 1} || Count <- CountList],
    User#user{count = NewCountList}.

%% @doc add
-spec add(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add(User = #user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add
-spec add(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber, total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, total_number = TotalNumber + Number, flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc add today
-spec add_today(User :: #user{}, Type :: non_neg_integer()) -> NewUser :: #user{}.
add_today(User = #user{role_id = RoleId, count = CountList}, Type) ->
    add_today(User = #user{role_id = RoleId, count = CountList}, Type, 1).

%% @doc add today
-spec add_today(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_today(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, flag = 1},
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
    NewCount = Count#count{total_number = TotalNumber + Number, flag = 1},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
