%%%------------------------------------------------------------------
%%% @doc
%%% module console
%%% @end
%%%------------------------------------------------------------------
-module(console).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4]).
-export([print_stacktrace/1, print_stacktrace/2]).
-export([format_stacktrace/1, format_stacktrace/2]).
-export([format/1, format/2]).
%% Macros
%% 忽略r16之前版本的控制台不支持颜色
-ifdef(DEBUG).
-define(IO(F), io:format(F)).
-define(IO(F, A), io:format(F, A)).
-else.
-define(IO(F), error_logger:error_msg(F)).
-define(IO(F, A), error_logger:error_msg(F, A)).
-endif.
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc 无颜色级别打印
-spec print(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
print(Module, Line, Format, Args) ->
    format_message("Print", fun(What) -> What end, Module, Line, Format, Args).

%% @doc 调试(蓝色)
-spec debug(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
debug(Module, Line, Format, Args) ->
    format_message("Debug", fun color:blue/1, Module, Line, Format, Args).

%% @doc 信息(绿色)
-spec info(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
info(Module, Line, Format, Args) ->
    format_message("Info", fun color:green/1, Module, Line, Format, Args).

%% @doc 警告(黄色)
-spec warming(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
warming(Module, Line, Format, Args) ->
    format_message("Warming", fun color:yellow/1, Module, Line, Format, Args).

%% @doc 错误(红色)
-spec error(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
error(Module, Line, Format, Args) ->
    format_message("Error", fun color:red/1, Module, Line, Format, Args).

%% format print/debug/info/warming/error message
format_message(Level, Color, Module, Line, Format, Args) ->
    %% windows console host color print not support
    FormatList = lists:flatten(lists:concat([Level, " [", Module, ":", Line, "] ", Color(Format), "~n"])),
    ?IO(FormatList, Args).

%% @doc 格式化stacktrace信息
-spec print_stacktrace(Stacktrace :: term()) -> ok | term().
print_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    ?IO(format_stacktrace(Reason, StackTrace));
print_stacktrace(Other) ->
    ?IO(Other).

%% @doc 格式化stacktrace信息
-spec print_stacktrace(Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Reason, StackTrace) ->
    ?IO(format_stacktrace(Reason, StackTrace)).

%% @doc 格式化stacktrace信息
-spec format_stacktrace(Stacktrace :: term()) -> ok | term().
format_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    format_stacktrace(Reason, StackTrace);
format_stacktrace(Other) ->
    io_lib:format("~p~n", [Other]).

%% @doc 格式化stacktrace信息
-spec format_stacktrace(Reason :: term(), Stacktrace :: term()) -> ok.
format_stacktrace(Reason, StackTrace) ->
    %% format exception reason
    ReasonMsg = format_reason(Reason),
    %% format exception stacktrace
    StackMsg = [io_lib:format("➡   ~s:~s(~ts:~p)~n", [Module, Function, FileName, Line]) || {Module, Function, _MethodLine, [{file, FileName}, {line, Line}]} <- StackTrace],
    %% format exception msg to tty/file
    lists:concat([ReasonMsg, StackMsg]).

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

%% @doc print to remote tty
-spec format(F :: string()) -> ok.
format(F) ->
    format(F, []).

%% @doc print to remote tty
-spec format(F :: string(), A :: [term()]) -> ok.
format(F, A) ->
    %% find remote group leader list
    LeaderList = lists:usort([element(2, erlang:process_info(shell:whereis_evaluator(X), group_leader)) || X <- erlang:processes(), shell:whereis_evaluator(X) =/= undefined]),
    %% io request
    PidList = [spawn(fun() -> io:format(Leader, F, A) end) || Leader <- LeaderList],
    %% kill it after 3 second if process block on io request
    spawn(fun() -> receive _ -> ok after 3000 -> [exit(Pid, kill) || Pid <- PidList] end end),
    ok.

%%%==================================================================
%%% Internal functions
%%%==================================================================
