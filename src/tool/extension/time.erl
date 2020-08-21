%%%-------------------------------------------------------------------
%%% @doc
%%% module time, manage time tick, to get timestamp
%%% erlang/calendar extended library
%%% before: read time from adjust ets time avoid erlang:now problem, but int otp 18 or later, this problem already fixed
%%% current: use new API erlang:timestamp instead
%%% @end
%%%-------------------------------------------------------------------
-module(time).
-compile({no_auto_import, [now/0]}).
%% API
-export([now/0, millisecond/0]).
-export([hour/0, hour/1]).
-export([zero/0, zero/1, day_hour/1, day_hour/2]).
-export([weekday/0, weekday/1, local_time/1]).
-export([is_same_day/2, is_same_week/2, is_same_month/2]).
-export([is_cross_day/1, is_cross_day/2, is_cross_day/3]).
-export([is_cross_week/1, is_cross_week/2, is_cross_week/3]).
-export([is_cross_weekday/2, is_cross_weekday/3, is_cross_weekday/4]).
-export([string/0, string/1]).
-export([format/1]).
-export([set_expire/1, set_expire/2, set_expire/3]).
-export([recover/4, recover/5, remain/2, remain/3, rotate/4, rotate/5]).
-export([send_after/2, start_timer/2, cancel_timer/1]).
-export([get_open_days/0]).
%% Includes
-include("common.hrl").
%% Macros
%% normal time define
-define(DIFF_SECONDS_0000_1900,                       59958230400).
-define(DIFF_SECONDS_1900_1970,                       2208988800).
-define(DIFF_SECONDS_0000_1970,                       62167219200).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc now (second timestamp)
-spec now() -> non_neg_integer().
now() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000 + Secs.

%% @doc millisecond timestamp
-spec millisecond() -> non_neg_integer().
millisecond() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

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
    {_, {Universal, _, _}} = erlang:universaltime(),
    {_, {Local, _, _}} = erlang:localtime(),
    Zero = Timestamp - (Timestamp + Local - Universal * ?HOUR_SECONDS) rem ?DAY_SECONDS,
    Zero + (Hour * ?HOUR_SECONDS).

%% @doc check tow timestamp is same day
-spec is_same_day(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_day(SecondsX, SecondsY) ->
    zero(SecondsX) == zero(SecondsY).

%% @doc check tow timestamp is same week
-spec is_same_week(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_week(SecondsX, SecondsY) ->
    BaseTimestamp = 1388937600,   %% 2014-01-06 00:00:0:0 Monday
    ((SecondsX - BaseTimestamp) div ?WEEK_SECONDS) == ((SecondsY - BaseTimestamp) div ?WEEK_SECONDS).

%% @doc check tow timestamp is same month
-spec is_same_month(SecondsX :: non_neg_integer(), SecondsY :: non_neg_integer()) -> boolean().
is_same_month(SecondsX, SecondsY) ->
    {{YearX, MonthX, _DayX}, _TimeX} = local_time(SecondsX),
    {{YearY, MonthY, _DayY}, _TimeY} = local_time(SecondsY),
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

%% @doc get weekday now
-spec weekday() -> non_neg_integer().
weekday() ->
    weekday(now()).

%% @doc get weekday by timestamp
-spec weekday(Timestamp :: non_neg_integer()) -> non_neg_integer().
weekday(Timestamp) ->
    {Date, _} = local_time(Timestamp),
    calendar:day_of_the_week(Date).

%% @doc timestamp to tuple time {{y, m, d}, {h, m, s}}
-spec local_time(Seconds :: non_neg_integer()) -> calendar:datetime().
local_time(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

%% @doc time string
-spec string() -> string().
string() ->
    string(now()).

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
-spec recover(Current :: non_neg_integer(), Limit :: non_neg_integer(), Before :: non_neg_integer(), CdTime :: non_neg_integer()) -> non_neg_integer().
recover(Current, Limit, CdTime, Before) ->
    recover(Current, Limit, CdTime, Before, now()).

%% @doc recover
-spec recover(Current :: non_neg_integer(), Limit :: non_neg_integer(), CdTime :: non_neg_integer(), Before :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
recover(Current, Limit, CdTime, Before, Now) ->
    erlang:min(Limit, Current + ((Now - Before) div CdTime)).

%% @doc remain
-spec remain(CdTime :: non_neg_integer(), Before :: non_neg_integer()) -> non_neg_integer().
remain(CdTime, Before) ->
    remain(CdTime, Before, now()).

%% @doc remain
-spec remain(CdTime :: non_neg_integer(), Before :: non_neg_integer(), Now :: non_neg_integer()) -> non_neg_integer().
remain(CdTime, Before, Now) ->
    CdTime - ((Now - Before) rem CdTime).

%% @doc rotate
-spec rotate(Current :: non_neg_integer(), Limit :: non_neg_integer(), Before :: non_neg_integer(), CdTime :: non_neg_integer()) -> {non_neg_integer(), non_neg_integer()}.
rotate(Current, Limit, CdTime, Before) ->
    rotate(Current, Limit, CdTime, Before, now()).

%% @doc rotate
-spec rotate(Current :: non_neg_integer(), Limit :: non_neg_integer(), CdTime :: non_neg_integer(), Before :: non_neg_integer(), Now :: non_neg_integer()) -> {non_neg_integer(), non_neg_integer()}.
rotate(Current, Limit, CdTime, Before, Now) ->
    {recover(Current, Limit, CdTime, Before, Now), remain(CdTime, Before, Now)}.

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

%% @doc get server open days
-spec get_open_days() -> non_neg_integer().
get_open_days() ->
    ((zero() + ?DAY_SECONDS - config:open_time()) div ?DAY_SECONDS).

%%%===================================================================
%%% Internal functions
%%%===================================================================
