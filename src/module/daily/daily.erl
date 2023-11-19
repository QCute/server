%%%-------------------------------------------------------------------
%%% @doc
%%% daily
%%% @end
%%%-------------------------------------------------------------------
-module(daily).
%% API
-export([on_load/1, on_save/1, on_reset/1]).
-export([on_count/2]).
-export([query_active/1, query/1]).
-export([award/2, award_active/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("daily.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Daily = daily_sql:select(RoleId),
    case daily_active_sql:select(RoleId) of
        [DailyActive] ->
            DailyActive;
        [] ->
            DailyActive = #daily_active{role_id = RoleId}
    end,
    User#user{daily = Daily, daily_active = DailyActive}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{daily = Daily, daily_active = DailyActive}) ->
    NewDaily = daily_sql:save(Daily),
    daily_active_sql:update(DailyActive),
    User#user{daily = NewDaily}.

%% @doc on reset
-spec on_reset(User :: #user{}) -> NewUser :: #user{}.
on_reset(User = #user{daily = DailyList, daily_active = DailyActive}) ->
    NewDailyList = [Daily#daily{is_award = 0, flag = 1} || Daily <- DailyList],
    NewDailyActive = DailyActive#daily_active{stage_id = 0, score = 0},
    User#user{daily = NewDailyList, daily_active = NewDailyActive}.

%% @doc query
-spec on_count(User :: #user{}, Event :: #event{}) -> #user{}.
on_count(User = #user{role_id = RoleId, daily = DailyList, daily_active = DailyActive}, #event{name = count, target = CountType, number = AddNumber}) ->
    case lists:keyfind(CountType, #daily.count_type, DailyList) of
        Daily = #daily{number = Number} ->
            Current = Number + AddNumber,
            NewDaily = Daily#daily{number = Current, flag = 1},
            user_sender:send(User, ?PROTOCOL_DAILY_QUERY, {DailyActive, [NewDaily]}),
            NewDailyList = lists:keystore(CountType, #daily.count_type, DailyList, NewDaily),
            User#user{daily = NewDailyList};
        _ ->
            AddDailyList = [
                #daily{role_id = RoleId, daily_id = DailyId, count_type = CountType, number = AddNumber, is_award = 0, flag = 1}
                ||
                #daily_data{daily_id = DailyId, count_type = Type} <- daily_data:list(),
                CountType == Type
            ],
            user_sender:send(User, ?PROTOCOL_DAILY_QUERY, {DailyActive, AddDailyList}),
            NewDailyList = listing:merge(AddDailyList, DailyList),
            User#user{daily = NewDailyList}
    end.

%% @doc query
-spec query(User :: #user{}) -> ok().
query_active(#user{daily_active = DailyActive}) ->
    {ok, DailyActive}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{daily = Daily}) ->
    {ok, Daily}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc award
-spec award(User :: #user{}, DailyId :: non_neg_integer()) -> ok() | error().
award(User, DailyId) ->
    case daily_data:get(DailyId) of
        DailyData = #daily_data{} ->
            receive_award(User, DailyData);
        _ ->
            {error, configure_not_found}
    end.

receive_award(User = #user{role_id = RoleId, daily = DailyList, daily_active = DailyActive = #daily_active{score = Score}}, #daily_data{daily_id = DailyId, number = NeedNumber, score = AddScore, award = Award}) ->
    %% TodayNumber = count:get_today(User, CountType),
    case listing:key_find(DailyId, #daily.daily_id, DailyList, #daily{role_id = RoleId, daily_id = DailyId, is_award = 1, flag = 1}) of
        Daily = #daily{number = TodayNumber, is_award = 0} when TodayNumber >= NeedNumber ->
            %% award
            {ok, NewUser} = item:add(User, Award, ?MODULE),
            %% update daily
            NewDaily = Daily#daily{is_award = 1, flag = 1},
            NewDailyList = lists:keystore(DailyId, #daily.daily_id, DailyList, NewDaily),
            %% update daily active score
            NewDailyActive = DailyActive#daily_active{score = Score + AddScore},
            user_sender:send(NewUser, ?PROTOCOL_DAILY_QUERY_ACTIVE, NewDailyActive),
            user_sender:send(NewUser, ?PROTOCOL_DAILY_QUERY, [NewDaily]),
            {ok, ok, NewUser#user{daily = NewDailyList, daily_active = NewDailyActive}};
        #daily{is_award = 1} ->
            {error, award_already_received};
        _ ->
            {error, daily_not_completed}
    end.

%% @doc award active
-spec award_active(User :: #user{}, StageId :: non_neg_integer()) -> ok() | error().
award_active(User = #user{daily_active = #daily_active{stage_id = PreId}}, StageId) ->
    case daily_active_data:get(StageId) of
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
            NewDailyActive = DailyActive#daily_active{stage_id = StageId},
            user_sender:send(NewUser, ?PROTOCOL_DAILY_QUERY_ACTIVE, NewDailyActive),
            {ok, ok, NewUser#user{daily_active = NewDailyActive}};
        false ->
            {error, daily_score_not_enough}
    end.
