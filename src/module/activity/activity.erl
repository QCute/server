%%%------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%------------------------------------------------------------------
-module(activity).
%% API
-export([server_start/1]).
-export([refresh/2]).
-export([query/0]).
-export([check/1, check/2, continue/3, continue/4, next_state/1, next_state/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("activity.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec server_start(NodeType :: atom()) -> {ok, term()}.
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
    erlang:send_after((WaitTime) * 1000, self(), {daily, Tomorrow}),
    %% reload all activity
    catch ets:delete_all_objects(?MODULE),
    refresh_loop(activity_data:list(), Now, NodeType),
    ok.

refresh_loop([], _, _) ->
    ok;
refresh_loop([ActivityId | T], Now, NodeType) ->
    case activity_data:get(ActivityId) of
        #activity_data{activity_id = ActivityId, mode = Mode, show_time = ShowTime, start_time = StartTime, over_time = OverTime, award_time = AwardTime, hide_time = HideTime, clean_time = CleanTime, service = Service} when Now =< ShowTime ->
            Activity = #activity{activity_id = ActivityId, start_time = StartTime, over_time = OverTime, award_time = AwardTime, hide_time = HideTime, clean_time = CleanTime},
            %% start activity server
            _ = Service =/= [] andalso Mode band NodeType =/= 0 andalso process:start(Service, [Activity]) == ok,
            %% notify server change activity state one second ago
            _ = Service =/= [] andalso Mode band NodeType =/= 0 andalso erlang:send_after(1000, Service, next_state) == ok,
            %% save  data
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
-spec check(ActivityId :: non_neg_integer(), State :: open | award) -> boolean().
check(ActivityId, State) when is_integer(ActivityId) ->
    check(hd(ets:lookup(?MODULE, ActivityId)), State);
check(#activity{activity_id = ActivityId, start_time = StartTime, over_time = OverTime}, open) ->
    Now = time:ts(),
    Hour = time:hour(Now),
    #activity_data{start_hour = StartHour, over_hour = OverHour} = activity_data:get(ActivityId),
    StartTime =< Now andalso Now < OverTime andalso StartHour =< Hour andalso Now < OverHour;
check(#activity{activity_id = ActivityId, award_time = AwardTime, hide_time = HideTime}, award) ->
    Now = time:ts(),
    Hour = time:hour(Now),
    #activity_data{start_award_hour = StartAwardHour, over_award_hour = OverAwardHour} = activity_data:get(ActivityId),
    AwardTime =< Now andalso Now < HideTime andalso StartAwardHour =< Hour andalso Now < OverAwardHour.

%% @doc continue next state
-spec continue(Activity :: #activity{}, function(), [term()]) -> term().
continue(Activity, Function, Args) ->
    {State, _, Time} = next_state(Activity),
    erlang:send_after(Time * 1000, self(), next_state),
    erlang:apply(Function, [State | Args]).

%% @doc continue next state
-spec continue(Activity :: #activity{}, module(), atom(), [term()]) -> term().
continue(Activity, Module, Function, Args) ->
    {State, _, Time} = next_state(Activity),
    erlang:send_after(Time * 1000, self(), next_state),
    erlang:apply(Module, Function, [State | Args]).

%% @doc get activity next state
%% 注意,如果多个时间相同,会直接跳到时间相同最后的那个状态
-spec next_state(Activity :: #activity{}) -> {atom(), atom(), non_neg_integer()}.
next_state(Activity = #activity{}) ->
    next_state(Activity, time:ts()).

%% @doc get activity next state
%% 注意,如果多个时间相同,会直接跳到时间相同最后的那个状态
-spec next_state(#activity{}, Now :: non_neg_integer()) -> {atom(), atom(), non_neg_integer()}.
next_state(#activity{show_time = ShowTime}, Now) when Now < ShowTime ->
    {wait, show, ShowTime - Now};
next_state(#activity{show_time = ShowTime, start_time = StartTime}, Now) when ShowTime =< Now andalso Now < StartTime ->
    {show, open, StartTime - Now};
next_state(#activity{start_time = StartTime, over_time = OverTime}, Now) when StartTime =< Now andalso Now < OverTime ->
    {open, over, OverTime - Now};
next_state(#activity{over_time = OverTime, award_time = AwardTime}, Now) when OverTime =< Now andalso Now < AwardTime ->
    {over, award, AwardTime - Now};
next_state(#activity{award_time = AwardTime, hide_time = HideTime}, Now) when AwardTime =< Now andalso Now < HideTime ->
    {award, hide, HideTime - Now};
next_state(#activity{hide_time = HideTime, clean_time = CleanTime}, Now) when HideTime =< Now andalso Now < CleanTime ->
    {hide, clean, CleanTime - Now};
next_state(#activity{clean_time = CleanTime}, Now) when CleanTime =< Now ->
    {clean, stop, 0}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
