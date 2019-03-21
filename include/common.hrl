%%%-------------------------------------------------------------------
%%% @doc
%%% common define
%%% @end
%%%-------------------------------------------------------------------

%% 时间相关
-define(MINUTE_SECONDS,                               60).            %% 一分钟的时间（秒）
-define(HOUR_SECONDS,                                 3600).          %% 一小时的时间（秒）
-define(DAY_SECONDS,                                  86400).         %% 一天的时间（秒）
-define(WEEK_SECONDS,                                 604800).        %% 一周的时间（秒）
-define(DIFF_SECONDS_0000_1900,                       59958230400).
-define(DIFF_SECONDS_1900_1970,                       2208988800).
-define(DIFF_SECONDS_0000_1970,                       62167219200).

%% 仅调试环境打印
-ifdef(DEBUG).
%% 打印
-define(PRINT(Msg),                                   catch console:print(?MODULE, ?LINE, Msg, [])).
-define(PRINT(Msg, Args),                             catch console:print(?MODULE, ?LINE, Msg, Args)).
%% 调试 (蓝色)
-define(DEBUG(Msg),                                   catch console:debug(?MODULE, ?LINE, Msg, [])).
-define(DEBUG(Msg, Args),                             catch console:debug(?MODULE, ?LINE, Msg, Args)).
-else.
%% 打印
-define(PRINT(Msg),                                   ok).
-define(PRINT(Msg, Args),                             ok).
%% 调试 (蓝色)
-define(DEBUG(Msg),                                   ok).
-define(DEBUG(Msg, Args),                             ok).
-endif.
%% 信息(绿色)
-define(INFO(Msg),                                    catch console:info(?MODULE, ?LINE, Msg, [])).
-define(INFO(Msg, Args),                              catch console:info(?MODULE, ?LINE, Msg, Args)).
%% 警告(黄色)
-define(WARMING(Msg),                                 catch console:warming(?MODULE, ?LINE, Msg, [])).
-define(WARMING(Msg, Args),                           catch console:warming(?MODULE, ?LINE, Msg, Args)).
%% 错误(红色)
-define(ERROR(Msg),                                   catch console:error(?MODULE, ?LINE, Msg, [])).
-define(ERROR(Msg, Args),                             catch console:error(?MODULE, ?LINE, Msg, Args)).

%% 打印 stack trace 信息
-define(STACKTRACE(Reason, Stacktrace),               catch console:stacktrace(Reason, Stacktrace)).
%% stack trace
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace),         Class:Reason:Stacktrace).
-define(GET_STACKTRACE(Stacktrace),                   Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _),                  Class:Reason).
-define(GET_STACKTRACE(_),                            erlang:get_stacktrace()).
-endif.

%% 数据库连接池名
-define(POOL,                                         pool).

%% 通用错误类型
-type error() :: {error, Code :: non_neg_integer()}.