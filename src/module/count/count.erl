%%%------------------------------------------------------------------
%%% @doc
%%% module count
%%% @end
%%%------------------------------------------------------------------
-module(count).
%% API
-export([load/1, save/1, reset/1]).
-export([add/3, add_today/3, add_total/3]).
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
    Count = parser:convert(count_sql:select(RoleId), ?MODULE),
    User#user{count = Count}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{count = Count}) ->
    count_sql:insert_update(Count),
    User.

%% @doc clean
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{count = CountList}) ->
    NewCountList = [Count#count{today_number = 0} || Count <- CountList],
    User#user{count = NewCountList}.

%% @doc add
-spec add(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber, total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number, total_number = TotalNumber + Number},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc add today
-spec add_today(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_today(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{today_number = TodayNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{today_number = TodayNumber + Number},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%% @doc add total
-spec add_total(User :: #user{}, Type :: non_neg_integer(), Number :: non_neg_integer()) -> NewUser :: #user{}.
add_total(User = #user{role_id = RoleId, count = CountList}, Type, Number) ->
    Count = #count{total_number = TotalNumber} = listing:key_find(Type, #count.type, CountList, #count{role_id = RoleId, type = Type}),
    NewCount = Count#count{total_number = TotalNumber + Number},
    NewCountList = lists:keystore(Type, #count.type, CountList, NewCount),
    User#user{count = NewCountList}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
