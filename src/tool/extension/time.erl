%%%-------------------------------------------------------------------
%%% @doc
%%% erlang time/calendar extended library
%%% before: read time from adjust ets time avoid erlang:now problem, but int otp 18 or later, this problem already fixed
%%% current: use new API erlang:timestamp/erlang:system_time instead
%%% @end
%%%-------------------------------------------------------------------
-module(time).
-compile({no_auto_import, [now/0]}).
%% API
-export([now/0, millisecond/0]).
-export([hour/0, hour/1]).
-export([zero/0, zero/1, day_hour/1, day_hour/2]).
-export([weekday/0, weekday/1]).
-export([is_same_day/2, is_same_week/2, is_same_month/2]).
-export([is_cross_day/1, is_cross_day/2, is_cross_day/3]).
-export([is_cross_week/1, is_cross_week/2, is_cross_week/3]).
-export([is_cross_weekday/2, is_cross_weekday/3, is_cross_weekday/4]).
-export([posix_time_to_local_time/1, local_time_to_posix_time/1]).
-export([timezone/0, timezone_offset/0]).
-export([format/0, format/1]).
-export([set_expire/1, set_expire/2, set_expire/3]).
-export([recover/4, recover/5]).
-export([get_open_days/0]).
%% Includes
-include("time.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc now (second timestamp)
-spec now() -> non_neg_integer().
now() ->
    erlang:system_time(second).

%% @doc millisecond timestamp
-spec millisecond() -> non_neg_integer().
millisecond() ->
    erlang:system_time(millisecond).

%% @doc now o'clock
-spec hour() -> non_neg_integer().
hour() ->
    hour(now()).

%% @doc timestamp o'clock
-spec hour(Timestamp :: non_neg_integer()) -> non_neg_integer().
hour(Timestamp) ->
    (Timestamp - zero(Timestamp)) div ?HOUR_SECONDS.

%% @doc zero time at now
-spec zero() -> non_neg_integer().
zero() ->
    zero(now()).

%% @doc zero time at this timestamp
-spec zero(Timestamp :: non_neg_integer()) -> non_neg_integer().
zero(Timestamp) ->
    day_hour(0, Timestamp).

%% @doc get day hour timestamp today
-spec day_hour(Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Hour) ->
    day_hour(Hour, now()).

%% @doc get day hour timestamp by timestamp
-spec day_hour(Timestamp :: non_neg_integer(), Hour :: non_neg_integer()) -> non_neg_integer().
day_hour(Hour, Timestamp) ->
    %% now zero time + hour seconds
    Timestamp - (Timestamp + timezone_offset()) rem ?DAY_SECONDS + ?HOUR_SECONDS(Hour).

%% @doc get weekday now
-spec weekday() -> non_neg_integer().
weekday() ->
    weekday(now()).

%% @doc get weekday by timestamp
-spec weekday(Timestamp :: non_neg_integer()) -> non_neg_integer().
weekday(Timestamp) ->
    {Date, _} = posix_time_to_local_time(Timestamp),
    calendar:day_of_the_week(Date).

%% @doc check tow timestamp is same day
-spec is_same_day(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_day(SecondsX, SecondsY) ->
    zero(SecondsX) == zero(SecondsY).

%% @doc check tow timestamp is same week
-spec is_same_week(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_week(SecondsX, SecondsY) ->
    BaseTimestamp = 631123200,   %% 1990-01-01 00:00:00 Monday
    ((SecondsX - BaseTimestamp) div ?WEEK_SECONDS) == ((SecondsY - BaseTimestamp) div ?WEEK_SECONDS).

%% @doc check tow timestamp is same month
-spec is_same_month(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_month(SecondsX, SecondsY) ->
    {{YearX, MonthX, _DayX}, _TimeX} = posix_time_to_local_time(SecondsX),
    {{YearY, MonthY, _DayY}, _TimeY} = posix_time_to_local_time(SecondsY),
    YearX == YearY andalso MonthX == MonthY.

%% @doc check is cross zero hour between before and now
-spec is_cross_day(Before :: non_neg_integer()) -> boolean().
is_cross_day(Before) ->
    is_cross_day(Before, 0).

%% @doc check is cross zero hour between before and now
-spec is_cross_day(Before :: non_neg_integer(), Hour :: non_neg_integer()) -> boolean().
is_cross_day(Before, Hour) ->
    is_cross_day(Before, Hour, now()).

%% @doc check is cross day hour between before and this timestamp
-spec is_cross_day(Before :: non_neg_integer(), Hour :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross_day(Before, Hour, Now) ->
    NowHour = day_hour(Hour, Now),
    Before =< NowHour andalso NowHour < Now.

%% @doc check is cross week hour between before and now
-spec is_cross_week(Before :: non_neg_integer()) -> boolean().
is_cross_week(Before) ->
    is_cross_week(Before, 0).

%% @doc check is cross week hour between before and now
-spec is_cross_week(Before :: non_neg_integer(), Hour :: non_neg_integer()) -> boolean().
is_cross_week(Before, Hour) ->
    is_cross_week(Before, Hour, now()).

%% @doc check is cross week hour between before and this timestamp
-spec is_cross_week(Before :: non_neg_integer(), Hour :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross_week(Before, Hour, Now) ->
    WeekdayHour = zero(Now) - ?DAY_SECONDS(weekday(Now) - 1) + ?HOUR_SECONDS(Hour),
    Before =< WeekdayHour andalso WeekdayHour < Now.

%% @doc check is cross week hour between before and now
-spec is_cross_weekday(Before :: non_neg_integer(), Weekday :: non_neg_integer()) -> boolean().
is_cross_weekday(Before, Weekday) ->
    is_cross_weekday(Before, Weekday, 0).

%% @doc check is cross week hour between before and now
-spec is_cross_weekday(Before :: non_neg_integer(), Weekday :: non_neg_integer(), Hour :: non_neg_integer()) -> boolean().
is_cross_weekday(Before, Weekday, Hour) ->
    is_cross_weekday(Before, Weekday, Hour, now()).

%% @doc check is cross week hour between before and this timestamp
-spec is_cross_weekday(Before :: non_neg_integer(), Weekday :: non_neg_integer(), Hour :: non_neg_integer(), Now :: non_neg_integer()) -> boolean().
is_cross_weekday(Before, Weekday, Hour, Now) ->
    WeekdayHour = zero(Now) - ?DAY_SECONDS(weekday(Now) - 1) + ?DAY_SECONDS(Weekday - 1) + ?HOUR_SECONDS(Hour),
    Before =< WeekdayHour andalso WeekdayHour < Now.

%% @doc timestamp to date time {{y, m, d}, {h, m, s}}
-spec posix_time_to_local_time(Seconds :: non_neg_integer()) -> calendar:datetime().
posix_time_to_local_time(Seconds) ->
    erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Seconds)).

%% @doc date time {{y, m, d}, {h, m, s}} to timestamp
-spec local_time_to_posix_time(Datetime :: calendar:datetime()) -> non_neg_integer().
local_time_to_posix_time(Datetime) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(Datetime)).

%% @doc time zone
-spec timezone() -> number().
timezone() ->
    timezone_offset() / 3600.

%% @doc time zone offset
-spec timezone_offset() -> integer().
timezone_offset() ->
    try
        persistent_term:get(?FUNCTION_NAME)
    catch _:_ ->
        LocalTime = erlang:localtime(),
        UniversalTime = erlang:localtime_to_universaltime(LocalTime),
        Offset = erlang:universaltime_to_posixtime(LocalTime) - erlang:universaltime_to_posixtime(UniversalTime),
        persistent_term:put(?FUNCTION_NAME, Offset),
        Offset
    end.

%% @doc now time format string
-spec format() -> string().
format() ->
    format(now()).

%% @doc format time to string Y-M-D H-M-S
-spec format(Timestamp :: non_neg_integer() | calendar:datetime1970()) -> string().
format(Timestamp) when is_integer(Timestamp) ->
    format(posix_time_to_local_time(Timestamp));
format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    binary_to_list(list_to_binary(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second]))).

%% @doc set expire time
-spec set_expire(EffectTime :: non_neg_integer()) -> non_neg_integer().
set_expire(EffectTime) ->
    set_expire(0, EffectTime, now()).

%% @doc set expire time
-spec set_expire(ExpireTime :: non_neg_integer(), EffectTime :: non_neg_integer()) -> non_neg_integer().
set_expire(ExpireTime, EffectTime) ->
    set_expire(ExpireTime, EffectTime, now()).

%% @doc set expire time
-spec set_expire(ExpireTime :: non_neg_integer(), EffectTime :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
set_expire(ExpireTime, 0, _) ->
    ExpireTime;
set_expire(0, EffectTime, Now) ->
    EffectTime + Now;
set_expire(ExpireTime, EffectTime, _) ->
    ExpireTime + EffectTime.

%% @doc recover
-spec recover(Current :: non_neg_integer(), Max :: non_neg_integer(), Before :: non_neg_integer(), Interval :: non_neg_integer()) -> {New :: non_neg_integer(), NextTime :: non_neg_integer()}.
recover(Current, Max, Interval, Before) ->
    recover(Current, Max, Interval, Before, now()).

%% @doc recover
-spec recover(Current :: non_neg_integer(), Max :: non_neg_integer(), Interval :: non_neg_integer(), Before :: non_neg_integer(), Now :: non_neg_integer()) -> {New :: non_neg_integer(), NextTime :: non_neg_integer()}.
recover(Current, Max, Interval, Before, Now) ->
    New = erlang:min(Max, Current + ((Now - Before) div Interval)),
    NextTime = Interval - ((Now - Before) rem Interval),
    {New, NextTime}.

%% @doc get server open days
-spec get_open_days() -> non_neg_integer().
get_open_days() ->
    ((zero() + ?DAY_SECONDS - config:open_time()) div ?DAY_SECONDS).

%%%===================================================================
%%% Internal functions
%%%===================================================================
