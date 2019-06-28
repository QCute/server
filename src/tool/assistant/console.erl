%%%-------------------------------------------------------------------
%%% @doc
%%% module console
%%% @end
%%%-------------------------------------------------------------------
-module(console).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4, stacktrace/1, stacktrace/2]).
%% Includes
-include("common.hrl").
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
print(Module, Line, Format, Args) ->
    format("Print", fun color:white/1, Module, Line, Format, Args).

%% @doc 调试(蓝色)
debug(Module, Line, Format, Args) ->
    format("Debug", fun color:blue/1, Module, Line, Format, Args).

%% @doc 信息(绿色)
info(Module, Line, Format, Args) ->
    format("Info", fun color:green/1, Module, Line, Format, Args).

%% @doc 警告(黄色)
warming(Module, Line, Format, Args) ->
    format("Warming", fun color:yellow/1, Module, Line, Format, Args).

%% @doc 错误(红色)
error(Module, Line, Format, Args) ->
    format("Error", fun color:red/1, Module, Line, Format, Args).

%% @doc 格式化stacktrace信息
stacktrace({'EXIT', {Reason, StackTrace}}) ->
    stacktrace(Reason, StackTrace);
stacktrace(Other) ->
    Other.

%% @doc 格式stacktrace化信息
stacktrace(Reason, StackTrace) ->
    %% format exception reason
    ReasonMsg = format_reason(Reason),
    %% format exception stacktrace
    StackMsg = [io_lib:format("    call from ~s:~s (file: ~ts,   line: ~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    %% format exception msg to tty/file
    ?IO(ReasonMsg ++ StackMsg).

%% format exception reason
format_reason({pool_error, {PoolId, Reason}}) ->
    io_lib:format("~ncatch exception: ~p(PoolId): ~p~n    ~s~n", [pool_error, PoolId, Reason]);
format_reason({sql_error, {Sql, ErrorCode, Reason}}) ->
    io_lib:format("~ncatch exception: ~p(ErrorCode): ~p~n    ~s~n    ~s~n", [sql_error, ErrorCode, Sql, Reason]);
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
%%%===================================================================
%%% Internal functions
%%%===================================================================
format(Level, _Color, Module, Line, Format, Args) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([" ", Y, "/", M, "/", D, " ", H, ":", I, ":", S, " "]),
    FormatList = lists:concat([Level, Date, "[", Module, ":", Line, "] ", Format, "~n"]),
    ?IO(FormatList, Args).
