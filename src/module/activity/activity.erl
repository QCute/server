%%%-------------------------------------------------------------------
%%% @doc
%%% module activity
%%% @end
%%%-------------------------------------------------------------------
-module(activity).
%% API
-export([server_start/1]).
-export([refresh/2]).
-export([query/0]).
-export([check/1, check/2, continue/1, continue/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("activity.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec server_start(NodeType :: non_neg_integer()) -> {ok, non_neg_integer()}.
server_start(NodeType) ->
    ets:new(?MODULE, [named_table, set, {keypos, #activity.activity_id}, {read_concurrency, true}]),
    Now = time:ts(),
    refresh(Now, NodeType),
    {ok, NodeType}.

%% @doc refresh activity daily
-spec refresh(Now :: non_neg_integer(), NodeType :: non_neg_integer()) -> ok.
refresh(Now, NodeType) ->
    %% tomorrow zero time
    Tomorrow = time:zero(Now) + ?DAY_SECONDS(1),
    WaitTime = Tomorrow - Now,
    erlang:send_after(?MILLISECONDS(WaitTime), self(), {daily, Tomorrow}),
    %% reload all activity
    ets:delete_all_objects(?MODULE),
    refresh_loop(activity_data:list(), Now, NodeType),
    ok.

refresh_loop([], _, _) ->
    ok;
refresh_loop([ActivityId | T], Now, NodeType) ->
    case activity_data:get(ActivityId) of
        #activity_data{activity_id = ActivityId, mode = Mode, show_time = ShowTime, start_time = StartTime, over_time = OverTime, award_time = AwardTime, stop_time = StopTime, service = Service} when ShowTime =< Now andalso Now =< StopTime ->
            Activity = #activity{activity_id = ActivityId, start_time = StartTime, over_time = OverTime, award_time = AwardTime, stop_time = StopTime},
            %% start activity server
            _ = Service =/= [] andalso Mode band NodeType =/= 0 andalso gen_server:start_link({local, Service}, Service, Activity, []) =/= ok,
            %% notify server change activity state one second ago
            _ = Service =/= [] andalso Mode band NodeType =/= 0 andalso erlang:send_after(?MILLISECONDS(1), Service, {activity, continue}) =/= ok,
            %% save data
            ets:insert(?MODULE, Activity),
            refresh_loop(T, Now, NodeType);
        _ ->
            refresh_loop(T, Now, NodeType)
    end.

%% @doc query
-spec query() -> ok().
query() ->
    {ok, ?MODULE}.

%% @doc check activity state
-spec check(ActivityId :: non_neg_integer()) -> boolean().
check(Activity) ->
    check(Activity, open).

%% @doc check activity state
-spec check(ActivityId :: non_neg_integer() | #activity{}, State :: open | award) -> boolean().
check(ActivityId, State) when is_integer(ActivityId) ->
    check(hd(ets:lookup(?MODULE, ActivityId)), State);
check(#activity{activity_id = ActivityId, start_time = StartTime, over_time = OverTime}, open) ->
    Now = time:ts(),
    Hour = time:hour(Now),
    #activity_data{start_hour = StartHour, over_hour = OverHour} = activity_data:get(ActivityId),
    StartTime =< Now andalso Now < OverTime andalso StartHour =< Hour andalso Now < OverHour;
check(#activity{activity_id = ActivityId, award_time = AwardTime, stop_time = StopTime}, award) ->
    Now = time:ts(),
    Hour = time:hour(Now),
    #activity_data{start_award_hour = StartAwardHour, over_award_hour = OverAwardHour} = activity_data:get(ActivityId),
    AwardTime =< Now andalso Now < StopTime andalso StartAwardHour =< Hour andalso Now < OverAwardHour.

%% @doc get activity next state
%% 注意,如果多个时间相同,会直接跳到时间相同最后的那个状态
-spec continue(Activity :: #activity{}) -> {atom(), reference() | undefined}.
continue(Activity = #activity{}) ->
    continue(Activity, time:ts()).

%% @doc get activity next state
%% 注意,如果多个时间相同,会直接跳到时间相同最后的那个状态
-spec continue(#activity{}, Now :: non_neg_integer()) -> {atom(), reference()}.
continue(#activity{show_time = ShowTime}, Now) when Now < ShowTime ->
    {wait, erlang:send_after(?MILLISECONDS(ShowTime - Now), self(), {activity, continue})};
continue(#activity{show_time = ShowTime, start_time = StartTime}, Now) when ShowTime =< Now andalso Now < StartTime ->
    {show, erlang:send_after(?MILLISECONDS(StartTime - Now), self(), {activity, continue})};
continue(#activity{start_time = StartTime, over_time = OverTime}, Now) when StartTime =< Now andalso Now < OverTime ->
    {start, erlang:send_after(?MILLISECONDS(OverTime - Now), self(), {activity, continue})};
continue(#activity{over_time = OverTime, award_time = AwardTime}, Now) when OverTime =< Now andalso Now < AwardTime ->
    {over, erlang:send_after(?MILLISECONDS(AwardTime - Now), self(), {activity, continue})};
continue(#activity{award_time = AwardTime, stop_time = StopTime}, Now) when AwardTime =< Now andalso Now < StopTime ->
    {award, erlang:send_after(?MILLISECONDS(StopTime - Now), self(), {activity, continue})};
continue(#activity{stop_time = StopTime}, Now) when StopTime =< Now ->
    {stop, undefined}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
