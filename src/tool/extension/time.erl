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
-export([zero/0, zero/1]).
-export([hour/0, hour/1, day_hour/1, day_hour/2, week_day/0, week_day/1, local_time/1]).
-export([is_same/3, is_same_day/2, is_same_week/2, is_same_month/2, is_cross/3, is_cross/4, is_cross_day/3, is_cross_week/3]).
-export([string/0, string/1]).
-export([format/1]).
-export([set_expire/1, set_expire/2, set_expire/3]).
-export([new_timer/0, add_timer/3, next_timer/1]).
-export([recover/4, recover/5, remain/2, remain/3, rotate/4, rotate/5]).
-export([send_after/2, start_timer/2, cancel_timer/1]).
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

%% @doc millis timestamp
-spec mts() -> non_neg_integer().
mts() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

%% @doc 零点时间戳
-spec zero() -> non_neg_integer().
zero() ->
    zero(ts()).

%% @doc 零点时间戳
-spec zero(Timestamp :: non_neg_integer()) -> non_neg_integer().
zero(Timestamp) ->
    day_hour(Timestamp, 0).

%% @doc now hour
-spec hour() -> non_neg_integer().
hour() ->
    hour(ts()).

%% @doc time hour
-spec hour(Timestamp :: non_neg_integer()) -> non_neg_integer().
hour(Timestamp) ->
    Zero = zero(Timestamp),
    (Timestamp - Zero) div ?HOUR_SECONDS.

%% @doc 获取指定时间当天几点的时间
-spec day_hour(Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Hour) ->
    day_hour(ts(), Hour).

%% @doc 获取指定时间当天几点的时间
-spec day_hour(Timestamp :: non_neg_integer(), Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Timestamp, Hour) ->
    %% TimeZone = config:time_zone(),
    TimeZone = parameter_data:get(time_zone),
    Zero = Timestamp - (Timestamp + TimeZone * ?HOUR_SECONDS) rem ?DAY_SECONDS,
    %% Zero = numeric:floor(Timestamp rem ?DAY_SECONDS),
    Zero + (Hour * ?HOUR_SECONDS).

%% @doc 判断时间是否同一天
-spec is_same(Type :: day | week | month, SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same(day, SecondsX, SecondsY) ->
    is_same_day(SecondsX, SecondsY);
is_same(week, SecondsX, SecondsY) ->
    is_same_week(SecondsX, SecondsY);
is_same(month, SecondsX, SecondsY) ->
    is_same_month(SecondsX, SecondsY).

%% @doc 判断时间是否同一天
-spec is_same_day(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_day(SecondsX, SecondsY) ->
    day_hour(SecondsX, 0) == day_hour(SecondsY, 0).

%% @doc 判断时间是否同一周
-spec is_same_week(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_week(SecondsX, SecondsY) ->
    BaseTimestamp = 1388937600,   %% 2014年1月6日0点0分0秒 星期一
    W1 = (SecondsX - BaseTimestamp) div ?WEEK_SECONDS,
    W2 = (SecondsY - BaseTimestamp) div ?WEEK_SECONDS,
    W1 =:= W2.

%% @doc 判断时间是否同一月
-spec is_same_month(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_month(SecondsX, SecondsY) ->
    {{YearX, MonthX, _DayX}, _TimeX} = local_time(SecondsX),
    {{YearY, MonthY, _DayY}, _TimeY} = local_time(SecondsY),
    YearX =:= YearY andalso MonthX =:= MonthY.

%% @doc 跨几点
-spec is_cross(Type :: day | week, Hour :: non_neg_integer(), LastTime :: non_neg_integer()) -> boolean().
is_cross(day, Hour, LastTime) ->
    is_cross_day(Hour, LastTime, ts());
is_cross(week, Hour, LastTime) ->
    is_cross_week(Hour, LastTime, ts()).

%% @doc 跨几点
-spec is_cross(Type :: day | week, Hour :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross(day, Hour, LastTime, Now) ->
    is_cross_day(Hour, LastTime, Now);
is_cross(week, Hour, LastTime, Now) ->
    is_cross_week(Hour, LastTime, Now).

%% @doc 跨每日几点
-spec is_cross_day(Hour :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross_day(Hour, LastTime, Now) ->
    LastHour = day_hour(LastTime, Hour),
    NowHour = day_hour(Now, Hour),
    %% 不在同一天，现在需要超过几点   在同一天，上次几点之前，下次几点之后
    (LastHour =/= NowHour andalso NowHour < Now) orelse (LastTime =< NowHour andalso NowHour < Now).

%% @doc 跨每周一几点
-spec is_cross_week(Hour :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross_week(Hour, LastTime, Now) ->
    NowHour = day_hour(Now, Hour),
    case week_day(Now) of
        1 when LastTime < NowHour andalso NowHour =< Now ->
            %% 星期一跨多少小时
            false;
        1 ->
            %% 星期一未跨多少小时
            true;
        _ ->
            is_same_week(LastTime, Now)
    end.

%% @doc 星期几
-spec week_day() -> non_neg_integer().
week_day() ->
    week_day(ts()).

-spec week_day(Timestamp :: non_neg_integer()) -> non_neg_integer().
week_day(Timestamp) ->
    {Date, _} = local_time(Timestamp),
    calendar:day_of_the_week(Date).

%% @doc 时间戳转日期
-spec local_time(Seconds :: non_neg_integer()) -> non_neg_integer().
local_time(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

%% @doc time string
-spec string() -> string().
string() ->
    string(ts()).

%% @doc time string
-spec string(Timestamp :: non_neg_integer() | erlang:timestamp()) -> string().
string(Timestamp) ->
    format(Timestamp).

%% @doc format time to string Y-M-D H-M-S
-spec format(Timestamp :: non_neg_integer() | erlang:timestamp() | calendar:datetime1970()) -> string().
format(Timestamp) when is_integer(Timestamp) ->
    format({Timestamp div 1000000, Timestamp rem 1000000, 0});
format(Timestamp = {_MegaSecs, _Secs, _MicroSecs}) ->
    LocalTime = calendar:now_to_local_time(Timestamp),
    format(LocalTime);
format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    binary_to_list(list_to_binary(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second]))).

%% @doc set expire time
-spec set_expire(EffectTime :: non_neg_integer()) -> non_neg_integer().
set_expire(EffectTime) ->
    set_expire(0, EffectTime, ts()).

%% @doc set expire time
-spec set_expire(ExpireTime :: non_neg_integer(), EffectTime :: non_neg_integer()) -> non_neg_integer().
set_expire(ExpireTime, EffectTime) ->
    set_expire(ExpireTime, EffectTime, ts()).

%% @doc set expire time
-spec set_expire(ExpireTime :: non_neg_integer(), EffectTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
set_expire(ExpireTime, 0, _) ->
    ExpireTime;
set_expire(0, EffectTime, Now) ->
    EffectTime + Now;
set_expire(ExpireTime, EffectTime, _) ->
    ExpireTime + EffectTime.

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
-spec recover(Current :: non_neg_integer(), Limit :: non_neg_integer(), LastTime :: non_neg_integer(), CdTime :: non_neg_integer()) -> non_neg_integer().
recover(Current, Limit, CdTime, LastTime) ->
    recover(Current, Limit, CdTime, LastTime, ts()).

%% @doc recover
-spec recover(Current :: non_neg_integer(), Limit :: non_neg_integer(), CdTime :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
recover(Current, Limit, CdTime, LastTime, Now) ->
    erlang:min(Limit, Current + ((Now - LastTime) div CdTime)).

%% @doc remain
-spec remain(CdTime :: non_neg_integer(), LastTime :: non_neg_integer()) -> non_neg_integer().
remain(CdTime, LastTime) ->
    remain(CdTime, LastTime, ts()).

%% @doc remain
-spec remain(CdTime :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
remain(CdTime, LastTime, Now) ->
    CdTime - ((Now - LastTime) rem CdTime).

%% @doc rotate
-spec rotate(Current :: non_neg_integer(), Limit :: non_neg_integer(), LastTime :: non_neg_integer(), CdTime :: non_neg_integer()) -> non_neg_integer().
rotate(Current, Limit, CdTime, LastTime) ->
    rotate(Current, Limit, CdTime, LastTime, ts()).

%% @doc rotate
-spec rotate(Current :: non_neg_integer(), Limit :: non_neg_integer(), CdTime :: non_neg_integer(), LastTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
rotate(Current, Limit, CdTime, LastTime, Now) ->
    {recover(Current, Limit, CdTime, LastTime, Now), remain(CdTime, LastTime, Now)}.

%% @doc send after seconds
-spec send_after(Time :: non_neg_integer(), Message :: term()) -> reference().
send_after(Time, _Message) when Time < 0 ->
    undefined;
send_after(Time, Message) ->
    erlang:send_after(?MILLISECONDS(Time), self(), Message).

%% @doc send after seconds
-spec start_timer(Time :: non_neg_integer(), Message :: term()) -> reference().
start_timer(Time, _Message) when Time < 0 ->
    undefined;
start_timer(Time, Message) ->
    erlang:start_timer(?MILLISECONDS(Time), self(), Message).

%% @doc cancel timer catch error
-spec cancel_timer(Timer :: reference()) -> non_neg_integer() | false.
cancel_timer(Timer) ->
    catch erlang:cancel_timer(Timer).

%%%==================================================================
%%% Internal functions
%%%==================================================================
