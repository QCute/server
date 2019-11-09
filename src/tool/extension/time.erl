%%%------------------------------------------------------------------
%%% @doc
%%% module time, manage time tick, to get timestamp
%%% erlang/calendar extended library
%%% before: read time from adjust ets time avoid erlang:now problem, but int otp 18 or later, this problem already fixed
%%% current: use new API erlang:timestamp instead
%%% @end
%%%------------------------------------------------------------------
-module(time).
-compile({no_auto_import, [now/0]}).
%% API
-export([ts/0, mts/0]).
-export([same/3, cross/4]).
-export([zero/0, zero/1]).
-export([hour/0, hour/1, day_hour/1, day_hour/2, week_day/0, week_day/1, local_time/1]).
-export([string/0, string/1]).
-export([format/1]).
-export([new_timer/0, add_timer/3, next_timer/1]).
-export([recover/5, remain/3]).
%% Includes
-include("common.hrl").
%% Macros
%% normal time define
-define(DIFF_SECONDS_0000_1900,                       59958230400).
-define(DIFF_SECONDS_1900_1970,                       2208988800).
-define(DIFF_SECONDS_0000_1970,                       62167219200).
%% Records
%% timer record
-record(timer, {ref, time, msg, list = []}).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc timestamp
-spec ts() -> non_neg_integer().
ts() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000 + Secs.

%% @doc timestamp
-spec mts() -> non_neg_integer().
mts() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000 + Secs + MicroSecs.

%% @doc 零点时间戳
-spec zero() -> non_neg_integer().
zero() ->
    zero(ts()).

%% @doc 零点时间戳
-spec zero(Hour :: non_neg_integer()) -> non_neg_integer().
zero(Now) ->
    day_hour(Now, 0).

%% @doc now hour
-spec hour() -> non_neg_integer().
hour() ->
    hour(ts()).

%% @doc now hour
-spec hour(Hour :: non_neg_integer()) -> non_neg_integer().
hour(Now) ->
    Zero = zero(Now),
    (Now - Zero) div ?HOUR_SECONDS.

%% @doc 获取指定时间当天几点的时间
-spec day_hour(Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Hour) ->
    day_hour(ts(), Hour).

%% @doc 获取指定时间当天几点的时间
-spec day_hour(Now :: non_neg_integer(), Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Now, Hour) ->
    TimeZone = config:time_zone(),
    Zero = Now - (Now + TimeZone * 3600) rem ?DAY_SECONDS,
    %% Zero = numeric:floor(Now rem ?DAY_SECONDS),
    Zero + (Hour * ?HOUR_SECONDS).

%% @doc 判断时间是否同一天/周
-spec same(atom(), SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
same(day, SecondsX, SecondsY) ->
    day_hour(SecondsX, 0) == day_hour(SecondsY, 0);
same(week, SecondsX, SecondsY) ->
    BaseTimestamp = 1388937600,   %% 2014年1月6日0点0分0秒 星期一
    W1 = (SecondsX - BaseTimestamp) div ?WEEK_SECONDS,
    W2 = (SecondsY - BaseTimestamp) div ?WEEK_SECONDS,
    W1 =:= W2;
same(month, SecondsX, SecondsY) ->
    {{YearX, MonthX, _DayX}, _TimeX} = local_time(SecondsX),
    {{YearY, MonthY, _DayY}, _TimeY} = local_time(SecondsY),
    YearX =:= YearY andalso MonthX =:= MonthY.

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
    week_day(erlang:timestamp()).
-spec week_day(Now::non_neg_integer()) -> non_neg_integer().
week_day(Now) ->
    {Date, _} = local_time(Now),
    calendar:day_of_the_week(Date).

%% @doc 时间戳转日期
-spec local_time(Seconds :: non_neg_integer()) -> non_neg_integer().
local_time(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

%% @doc time string
-spec string() -> string().
string() ->
    string(erlang:timestamp()).

%% @doc time string
-spec string(Now :: non_neg_integer() | erlang:timestamp()) -> string().
string(Now) ->
    format(Now).

%% @doc format time to string Y-M-D H-M-S
-spec format(Time :: non_neg_integer() | erlang:timestamp() | calendar:datetime1970()) -> string().
format(Now) when is_integer(Now) ->
    format({Now div 1000000, Now rem 1000000, 0});
format(Now = {_MegaSecs, _Secs, _MicroSecs}) ->
    LocalTime = calendar:now_to_local_time(Now),
    format(LocalTime);
format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    binary_to_list(list_to_binary(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second]))).

%% @doc new timer
-spec new_timer() -> #timer{}.
new_timer() ->
    #timer{}.

%% @doc add timer(recent one first)
-spec add_timer(Timer :: #timer{}, Time :: non_neg_integer(), Msg :: term()) -> NewTimer :: #timer{}.
add_timer(Timer = #timer{ref = undefined}, Time, Msg) ->
    Ref = erlang:send_after(Time, self(), Msg),
    Timer#timer{ref = Ref, time = Time + mts(), msg = Msg};
add_timer(Timer = #timer{ref = LastRef, time = LastTime, msg = LastMsg, list = List}, Time, Msg) ->
    Now = mts(),
    case erlang:read_timer(LastRef) of
        false ->
            Ref = erlang:send_after(Time, self(), Msg),
            Timer#timer{ref = Ref, time = Now + Time, msg = Msg};
        RemainTime when Time < RemainTime ->
            erlang:cancel_timer(LastRef),
            Ref = erlang:send_after(Time, self(), Msg),
            NewList = lists:sort(fun({X, _}, {Y, _}) -> X < Y end, [{LastTime, LastMsg} | List]),
            Timer#timer{list = NewList, ref = Ref, time = Now + Time, msg = Msg};
        _ ->
            NewList = lists:sort(fun({X, _}, {Y, _}) -> X < Y end, [{Now + Time, Msg} | List]),
            Timer#timer{list = NewList}
    end.

%% @doc add next timer
-spec next_timer(Timer :: #timer{}) -> NewTimer :: #timer{}.
next_timer(Timer = #timer{list = []}) ->
    Timer;
next_timer(Timer = #timer{time = LastTime, list = [{Time, Request} | T]}) ->
    NewRef = erlang:send_after(Time - LastTime, self(), Request),
    Timer#timer{list = T, ref = NewRef, time = Time, msg = Request}.

%% @doc recover
-spec recover(Current:: non_neg_integer(), Limit :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer(), CdTime :: non_neg_integer()) -> non_neg_integer().
recover(Current, Limit, LastTime, Now, CdTime) ->
    case Limit =< Current of
        true ->
            Limit;
        false ->
            Total = Current + ((Now - LastTime) div CdTime),
            case Limit =< Total of
                true ->
                    Limit;
                false ->
                    Total
            end
    end.

%% @doc remain
-spec remain(Time :: non_neg_integer(), CdTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
remain(Time, CdTime, Now) ->
    RefreshTime = Time + CdTime,
    case 0 < Time of
        true ->
            case Now < RefreshTime of
                true ->
                    RefreshTime - Now;
                false ->
                    CdTime - ((Now - Time) rem CdTime)
            end;
        false ->
            0
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
