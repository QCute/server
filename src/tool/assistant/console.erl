%%%-------------------------------------------------------------------
%%% @doc
%%% module console
%%% @end
%%%-------------------------------------------------------------------
-module(console).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4, stacktrace/1, stacktrace/2]).
%% Macros
%% 忽略r16之前版本的控制台不支持颜色
-ifdef(DEBUG).
-define(IO(F), io:format(F)).
-define(IO(F, A), io:format(F, A)).
-else.
-define(IO(F), error_logger:error_msg(F)).
-define(IO(F, A), error_logger:error_msg(F, A)).
-endif.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 无颜色级别打印
-spec print(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
print(Module, Line, Format, Args) ->
    format("Print", fun(What) -> What end, Module, Line, Format, Args).

%% @doc 调试(蓝色)
-spec debug(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
debug(Module, Line, Format, Args) ->
    format("Debug", fun color:blue/1, Module, Line, Format, Args).

%% @doc 信息(绿色)
-spec info(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
info(Module, Line, Format, Args) ->
    format("Info", fun color:green/1, Module, Line, Format, Args).

%% @doc 警告(黄色)
-spec warming(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
warming(Module, Line, Format, Args) ->
    format("Warming", fun color:yellow/1, Module, Line, Format, Args).

%% @doc 错误(红色)
-spec error(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
error(Module, Line, Format, Args) ->
    format("Error", fun color:red/1, Module, Line, Format, Args).

%% @doc 格式化stacktrace信息
-spec stacktrace(Stacktrace :: term()) -> ok | term().
stacktrace({'EXIT', {Reason, StackTrace}}) ->
    stacktrace(Reason, StackTrace);
stacktrace(Other) ->
    Other.

%% @doc 格式stacktrace化信息
-spec stacktrace(Reason :: term(), Stacktrace :: term()) -> ok | term().
stacktrace(Reason, StackTrace) ->
    %% format exception reason
    ReasonMsg = format_reason(Reason),
    %% format exception stacktrace
    StackMsg = [io_lib:format("    call from ~s:~s (file: ~ts,   line: ~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    %% format exception msg to tty/file
    ?IO(ReasonMsg ++ StackMsg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% format exception reason
format_reason({pool_error, {PoolId, Reason}}) ->
    io_lib:format("~ncatch exception: ~p(PoolId): ~p~n    ~s~n", [pool_error, PoolId, Reason]);
format_reason({sql_error, {Sql, ErrorCode, Reason}}) ->
    io_lib:format("~ncatch exception: ~p(ErrorCode): ~p~n    sql: ~s~n    reason: ~s~n", [sql_error, ErrorCode, Sql, Reason]);
format_reason({badmatch, Match}) ->
    io_lib:format("~ncatch exception: ~p   ~p~n", [badmatch, Match]);
format_reason({case_clause, Match}) ->
    io_lib:format("~ncatch exception: ~p   ~p~n", [case_clause, Match]);
format_reason({function_clause, [{M, F, A}]}) ->
    AF = string:join(lists:duplicate(length(A), "~p"), ", "),
    io_lib:format("~ncatch exception: ~p ~p:~p(" ++ AF ++ ")~n", [function_clause, M, F | A]);
format_reason({noproc, {M, F, A}}) ->
    AF = string:join(lists:duplicate(length(A), "~p"), ", "),
    io_lib:format("~ncatch exception: ~p ~p:~p(" ++ AF ++ ")~n", [noproc, M, F | A]);
format_reason(Reason) ->
    io_lib:format("~ncatch exception: ~p~n", [Reason]).

%% format stacktrace
format(Level, Color, Module, Line, Format, Args) ->
    %% windows console host color print not support
    FormatList = lists:flatten(lists:concat([Level, " ", time:string(), " ", "[", Module, ":", Line, "] ", Color(Format), "~n"])),
    ?IO(FormatList, Args).
