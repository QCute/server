%%%-------------------------------------------------------------------
%%% @doc
%%% module time, manage time tick, to get timestamp
%%% @end
%%%-------------------------------------------------------------------
-module(time_server).
-compile(nowarn_deprecated_function).
-behaviour(gen_server).
-include("common.hrl").
%% API
-export([ts/0, same/3, cross/4]).
-export([day_hour/1, day_hour/2, week_day/0, week_day/1, local_time/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state record
-record(state, {offset = 0}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc timestamp
-spec ts() -> non_neg_integer().
ts() ->
    [{timer, {Now, _}}] = ets:lookup(timer, timer),
    {MegaSecs, Secs, _MicroSecs} = Now,
    MegaSecs * 1000000 + Secs.

%% @doc 获取指定时间当天几点的时间
-spec day_hour(Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Hour) ->
    day_hour(ts(), Hour).
-spec day_hour(Now :: non_neg_integer(), Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Now, Hour) ->
    Zero = tool:floor(Now / ?DAY_SECONDS),
    Zero + (Hour * ?HOUR_SECONDS).

%% @doc 判断时间是否同一天/周
-spec same(atom(), SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
same(day, SecondsX, SecondsY) ->
    day_hour(SecondsX, 0) == day_hour(SecondsY, 0);
same(week, SecondsX, SecondsY) ->
    BaseT = 1388937600,   %% 2014年1月6日0点0分0秒 星期一
    W1 = (SecondsX - BaseT) div ?WEEK_SECONDS,
    W2 = (SecondsY - BaseT) div ?WEEK_SECONDS,
    W1 =:= W2.

%% @doc 跨凌晨几点
-spec cross(atom(), Hour :: non_neg_integer(), LastTime :: non_neg_integer(), Now::non_neg_integer()) -> boolean().
cross(day, Hour, LastTime, Now) ->
    LastHour = day_hour(LastTime, Hour),
    NowHour = day_hour(Now, Hour),
    %% 不在同一天，现在需要超过几点   在同一天，上次几点之前，下次几点之后
    (LastHour =/= NowHour andalso NowHour < Now) orelse (LastTime =< NowHour andalso NowHour < Now);
cross(week, Hour, LastTime, Now) ->
    NowHour = day_hour(Now, Hour),
    case week_day(Now) of
        1 when LastTime < NowHour andalso NowHour =< Now ->
            %% 星期一跨5点
            false;
        1 ->
            %% 星期一未跨5点
            true;
        _ ->
            same(week, LastTime, Now)
    end.

%% @doc 星期几
-spec week_day() -> non_neg_integer().
week_day() ->
    week_day(ts()).
-spec week_day(Now::non_neg_integer()) -> non_neg_integer().
week_day(Now) ->
    {Date, _} = local_time(Now),
    calendar:day_of_the_week(Date).

%% @doc 时间戳转日期
-spec local_time(Seconds :: non_neg_integer()) -> non_neg_integer().
local_time(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

%% @doc server start
start() ->
    process:start(?MODULE).

%% @doc server start
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(timer, [set, protected, named_table]),
    ets:insert(timer, {timer, {erlang:now(), 0}}),
    Day = {{2000, 1, 1}, {0, 0, 0}},
    [UTC] = calendar:local_time_to_universal_time_dst(Day),
    Tick1 = calendar:datetime_to_gregorian_seconds(Day),
    Tick2 = calendar:datetime_to_gregorian_seconds(UTC),
    ets:insert(timer, {dst, Tick2 - Tick1}),
    erlang:send_after(1000, self(), {event, clock}),
    {ok, #state{offset = 0}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({event, clock}, State = #state{offset = Offset}) ->
    erlang:send_after(1000, self(), {event, clock}),
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    NewTimeCall = {MegaSecs, Secs + Offset, _MicroSecs},
    ets:insert(timer, {timer, {NewTimeCall, Time_Since_Last_Call}}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
