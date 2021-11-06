%%%-------------------------------------------------------------------
%%% @doc
%%% daily
%%% @end
%%%-------------------------------------------------------------------
-module(daily).
%% API
-export([load/1, save/1, reset/1]).
-export([query_count/1, query/1]).
-export([award/2, award_active/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("count.hrl").
-include("daily.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Daily = daily_sql:select_by_role_id(RoleId),
    case daily_active_sql:select(RoleId) of
        [DailyActive] ->
            DailyActive;
        [] ->
            DailyActive = #daily_active{role_id = RoleId}
    end,
    User#user{daily = Daily, daily_active = DailyActive}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{daily = Daily, daily_active = DailyActive}) ->
    NewDaily = daily_sql:insert_update(Daily),
    daily_active_sql:update(DailyActive),
    User#user{daily = NewDaily}.

%% @doc reset
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{daily = DailyList, daily_active = DailyActive}) ->
    NewDailyList = [Daily#daily{is_award = 0, flag = 1} || Daily <- DailyList],
    NewDailyActive = DailyActive#daily_active{stage_id = 0, score = 0},
    User#user{daily = NewDailyList, daily_active = NewDailyActive}.

%% @doc query
-spec query_count(User :: #user{}) -> ok().
query_count(#user{count = Count}) ->
    {ok, Count}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{daily = Daily, daily_active = DailyActive}) ->
    {ok, [Daily, DailyActive]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc award
-spec award(User :: #user{}, DailyId :: non_neg_integer()) -> ok() | error().
award(User, DailyId) ->
    case daily_data:get_daily(DailyId) of
        DailyData = #daily_data{} ->
            receive_award(User, DailyData);
        _ ->
            {error, configure_not_found}
    end.

receive_award(User = #user{role_id = RoleId, daily = DailyList, daily_active = DailyActive = #daily_active{score = Score}}, #daily_data{daily_id = DailyId, count_type = CountType, number = NeedNumber, score = AddScore, award = Award}) ->
    TodayNumber = count:get_today(User, CountType),
    case listing:key_find(DailyId, #daily.daily_id, DailyList, #daily{role_id = RoleId, daily_id = DailyId, is_award = 1, flag = 1}) of
        Daily = #daily{is_award = 0} when TodayNumber >= NeedNumber ->
            %% award
            {ok, NewUser} = item:add(User, Award, ?MODULE),
            %% update daily
            NewDailyList = lists:keystore(DailyId, #daily.daily_id, DailyList, Daily),
            %% update daily active score
            NewDailyActive = DailyActive#daily_active{score = Score + AddScore},
            {ok, ok, NewUser#user{daily = NewDailyList, daily_active = NewDailyActive}};
        #daily{is_award = 1} ->
            {error, award_already_received};
        _ ->
            {error, daily_not_completed}
    end.

%% @doc award active
-spec award_active(User :: #user{}, StageId :: non_neg_integer()) -> ok() | error().
award_active(User = #user{daily_active = #daily_active{stage_id = PreId}}, StageId) ->
    case daily_data:get_daily_active(StageId) of
        DailyActiveData = #daily_active_data{pre_id = PreId} ->
            receive_award_active(User, DailyActiveData);
        #daily_active_data{} ->
            %% previous award not received
            {error, award_pre_not_received};
        _ ->
            {error, configure_not_found}
    end.

receive_award_active(User = #user{daily_active = DailyActive = #daily_active{score = Score}}, #daily_active_data{stage_id = StageId, score = NeedScore, award = Award}) ->
    case Score >= NeedScore of
        true ->
            {ok, NewUser} = item:add(User, Award, ?MODULE),
            {ok, ok, NewUser#user{daily_active = DailyActive#daily_active{stage_id = StageId}}};
        false ->
            {error, daily_score_not_enough}
    end.
