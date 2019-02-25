%%%-------------------------------------------------------------------
%%% @doc
%%% module console
%%% @end
%%%-------------------------------------------------------------------
-module(console).
-export([print/4, debug/4, info/4, warming/4, error/4, stack_trace/2]).
-include("common.hrl").
%% 忽略r16之前版本的控制台不支持颜色
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

%% @doc 格式化catch信息
stack_trace(pool_error, {PoolId, Reason}) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s(PoolId): ~p~n    ~s~n", [pool_error, PoolId, Reason]),
    io:format(ReasonMsg),
    ok;
stack_trace(sql_error, {Sql, ErrorCode, Reason}) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s(ErrorCode): ~p~n    ~s~n    ~s~n", [sql_error, ErrorCode, Sql, Reason]),
    io:format(ReasonMsg),
    ok;
stack_trace(noproc, {Module, Function, Args}) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s   ~n~p, ~p, ~p~n", [noproc, Module, Function, Args]),
    io:format(ReasonMsg),
    ok;
stack_trace({badmatch, Match}, StackTrace) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s   ~p~n", [badmatch, Match]),
    StackMsg = [io_lib:format("    call from ~s:~s (file: ~ts,   line: ~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    io:format(ReasonMsg ++ StackMsg),
    ok;
stack_trace({case_clause, Match}, StackTrace) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s   ~s~n", [case_clause, Match]),
    StackMsg = [io_lib:format("    call from ~s:~s (file: ~ts,   line: ~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    io:format(ReasonMsg ++ StackMsg),
    ok;
stack_trace(Reason, StackTrace) ->
    ReasonMsg = io_lib:format("~ncatch exception: ~s~n", [Reason]),
    StackMsg = [io_lib:format("    call from ~s:~s (file: ~ts,   line: ~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    io:format(ReasonMsg ++ StackMsg),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
format(Level, _Color, Module, Line, Format, Args) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([" ", Y, "/", M, "/", D, " ", H, ":", I, ":", S, " "]),
    FormatList = lists:concat(["~s", Level, Date, "[", Module, ":", Line, "] ", Format, "~s~n"]),
    io:format(FormatList, Args).
