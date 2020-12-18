%%%-------------------------------------------------------------------
%%% @doc
%%% common define
%%% @end
%%%-------------------------------------------------------------------
%% 布尔值定义
-define(TRUE,                                         1).
-define(FALSE,                                        0).

%% 时间相关
%% 毫秒 定时器使用
-define(MILLISECONDS(Seconds),                        ((Seconds) * 1000)).       %% 一秒的时间（毫秒）
-define(MINUTE_MILLISECONDS(Minute),                  ((Minute) * 60 * 1000)).   %% 一分钟的时间（毫秒）
-define(HOUR_MILLISECONDS(Hour),                      ((Hour) * 3600 * 1000)).   %% 一分钟的时间（毫秒）
-define(DAY_MILLISECONDS(Day),                        ((Day) * 86400 * 1000)).   %% 一天的时间（毫秒）
-define(WEEK_MILLISECONDS(Week),                      ((Week) * 604800 * 1000)). %% 一周的时间（毫秒）

%% 秒 一般记录使用
-define(MINUTE_SECONDS(Minute),                       ((Minute) * 60)).          %% 一分钟的时间（秒）
-define(HOUR_SECONDS(Hour),                           ((Hour) * 3600)).          %% 一分钟的时间（秒）
-define(DAY_SECONDS(Day),                             ((Day) * 86400)).          %% 一天的时间（秒）
-define(WEEK_SECONDS(Week),                           ((Week) * 604800)).        %% 一周的时间（秒）

%% 毫秒 定时器使用
-define(MILLISECONDS,                                 1000).                     %% 一秒的时间（毫秒）
-define(MINUTE_MILLISECONDS,                          60 * 1000).                %% 一分钟的时间（毫秒）
-define(HOUR_MILLISECONDS,                            3600 * 1000).              %% 一小时的时间（毫秒）
-define(DAY_MILLISECONDS,                             86400 * 1000).             %% 一天的时间（毫秒）
-define(WEEK_MILLISECONDS,                            604800 * 1000).            %% 一周的时间（毫秒）

%% 秒 一般记录使用
-define(MINUTE_SECONDS,                               60).                       %% 一分钟的时间（秒）
-define(HOUR_SECONDS,                                 3600).                     %% 一小时的时间（秒）
-define(DAY_SECONDS,                                  86400).                    %% 一天的时间（秒）
-define(WEEK_SECONDS,                                 604800).                   %% 一周的时间（秒）

-define(CALL_TIMEOUT,                                 ?MILLISECONDS(5)).         %% call默认超时

%% 打印(无颜色)
-define(PRINT(Msg),                                   catch console:print(?MODULE, ?LINE, Msg, [])).
-define(PRINT(Msg, Args),                             catch console:print(?MODULE, ?LINE, Msg, Args)).
%% 仅调试环境打印
-ifdef(DEBUG).
%% 调试 (蓝色)
-define(DEBUG_MSG(Msg),                               catch console:debug(?MODULE, ?LINE, Msg, [])).
-define(DEBUG_MSG(Msg, Args),                         catch console:debug(?MODULE, ?LINE, Msg, Args)).
-else.
%% 调试 (蓝色)
-define(DEBUG_MSG(Msg),                               ok).
-define(DEBUG_MSG(Msg, Args),                         ok).
-endif.
%% 信息(绿色)
-define(INFO_MSG(Msg),                                catch console:info(?MODULE, ?LINE, Msg, [])).
-define(INFO_MSG(Msg, Args),                          catch console:info(?MODULE, ?LINE, Msg, Args)).
%% 警告(黄色)
-define(WARMING_MSG(Msg),                             catch console:warming(?MODULE, ?LINE, Msg, [])).
-define(WARMING_MSG(Msg, Args),                       catch console:warming(?MODULE, ?LINE, Msg, Args)).
%% 错误(红色)
-define(ERROR_MSG(Msg),                               catch console:error(?MODULE, ?LINE, Msg, [])).
-define(ERROR_MSG(Msg, Args),                         catch console:error(?MODULE, ?LINE, Msg, Args)).

%% 打印 stack trace 信息
-define(STACKTRACE(Reason, Stacktrace),               catch console:print_stacktrace(Reason, Stacktrace)).
%% stack trace
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace),         Class:Reason:Stacktrace).
-define(GET_STACKTRACE(Stacktrace),                   Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _),                  Class:Reason).
-define(GET_STACKTRACE(_),                            erlang:get_stacktrace()).
-endif.

%% 通用错误类型
-type error() :: error | {error, term()}.
