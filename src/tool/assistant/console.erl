%%%-------------------------------------------------------------------
%%% @doc
%%% console print tool
%%% @end
%%%-------------------------------------------------------------------
-module(console).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4]).
-export([print_stacktrace/1, print_stacktrace/2]).
-export([format_stacktrace/1, format_stacktrace/2]).
-export([format/1, format/2]).
-export([set_prompt/0, prompt_func/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc print(default)
-spec print(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
print(Module, Line, Format, Args) ->
    format_message("Print", fun(What) -> What end, Module, Line, Format, Args).

%% @doc debug(blue)
-spec debug(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
debug(Module, Line, Format, Args) ->
    format_message("Debug", fun color:blue/1, Module, Line, Format, Args).

%% @doc info(green)
-spec info(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
info(Module, Line, Format, Args) ->
    format_message("Info", fun color:green/1, Module, Line, Format, Args).

%% @doc warming(yellow)
-spec warming(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
warming(Module, Line, Format, Args) ->
    format_message("Warming", fun color:yellow/1, Module, Line, Format, Args).

%% @doc error(red)
-spec error(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
error(Module, Line, Format, Args) ->
    format_message("Error", fun color:red/1, Module, Line, Format, Args).

%% format print/debug/info/warming/error message
format_message(Level, Color, Module, Line, Format, Args) ->
    %% windows console host color print not support
    FormatList = lists:flatten(lists:concat([Level, " [", Module, ":", Line, "] ", "[", time:format(), "] ", Color(Format), "~n"])),
    error_logger:error_msg(FormatList, Args).

%% @doc print formatted stacktrace message
-spec print_stacktrace(Stacktrace :: term()) -> ok | term().
print_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    error_logger:error_msg(format_stacktrace(Reason, StackTrace));
print_stacktrace(Other) ->
    error_logger:error_msg(Other).

%% @doc print formatted stacktrace message
-spec print_stacktrace(Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Reason, StackTrace) ->
    error_logger:error_msg(format_stacktrace(Reason, StackTrace)).

%% @doc print formatted stacktrace message
-spec format_stacktrace(Stacktrace :: term()) -> string().
format_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    format_stacktrace(Reason, StackTrace);
format_stacktrace(Other) ->
    io_lib:format("~0p~n", [Other]).

%% @doc format stacktrace message
-spec format_stacktrace(Reason :: term(), Stacktrace :: term()) -> string().
format_stacktrace(Reason, StackTrace) ->
    %% format exception reason
    ReasonMsg = format_reason(Reason, StackTrace),
    %% format exception stacktrace
    StackMsg = [format_stacktrace_msg(Stack) || Stack <- StackTrace],
    %% format exception msg to tty/file
    io_lib:format("~ts~ts", [ReasonMsg, StackMsg]).

%% stack trace message ref: erlang:stack_item
format_stacktrace_msg({Module, Function, Arity, []}) when is_integer(Arity) ->
    io_lib:format("➡   ~s:~s/~w~n", [Module, Function, Arity]);
format_stacktrace_msg({Module, Function, Arity, [{file, FileName}, {line, Line}]}) when is_integer(Arity) ->
    io_lib:format("➡   ~s:~s/~w(~s:~w)~n", [Module, Function, Arity, FileName, Line]);
format_stacktrace_msg({Module, Function, Args, []}) ->
    AF = string:join(lists:duplicate(length(Args), "~0p"), ", "),
    io_lib:format("➡   ~s:~s(" ++ AF ++ ")~n", [Module, Function | Args]);
format_stacktrace_msg({Module, Function, Args, [{file, FileName}, {line, Line}]}) ->
    AF = string:join(lists:duplicate(length(Args), "~0p"), ", "),
    io_lib:format("➡   ~s:~s(" ++ AF ++ ")(~s:~w)~n", [Module, Function | Args] ++ [FileName, Line]).

%% format exception reason
format_reason({pool_error, {PoolId, Reason}}, _) ->
    io_lib:format("~ncatch exception: ~w(PoolId): ~w~n    ~w~n", [pool_error, PoolId, Reason]);
format_reason({mysql_error, {Sql, ErrorCode, Reason}}, _) ->
    io_lib:format("~ncatch exception: ~w~nErrorCode: ~w~nsql: ~s~nreason: ~s~n", [mysql_error, ErrorCode, Sql, Reason]);
format_reason({badmatch, Match}, _) ->
    io_lib:format("~ncatch exception: ~w: ~w~n", [badmatch, Match]);
format_reason({case_clause, Match}, _) ->
    io_lib:format("~ncatch exception: ~w: ~w~n", [case_clause, Match]);
format_reason(function_clause, _) ->
    io_lib:format("~ncatch exception: ~w ~n", [function_clause]);
format_reason(badarg, _) ->
    io_lib:format("~ncatch exception: ~w ~n", [badarg]);
format_reason(undef, _) ->
    io_lib:format("~ncatch exception: ~w ~n", [undef]);
format_reason(noproc, _) ->
    io_lib:format("~ncatch exception: ~w ~n", [noproc]);
format_reason(Reason, _) ->
    io_lib:format("~ncatch exception: ~0p~n", [Reason]).

%% @doc print to remote tty
-spec format(Format :: string()) -> ok.
format(Format) ->
    format(Format, []).

%% @doc print to remote tty
-spec format(Format :: string(), Args :: [term()]) -> ok.
format(Format, Args) ->
    %% find remote group leader list
    LeaderList = lists:usort([element(2, erlang:process_info(shell:whereis_evaluator(X), group_leader)) || X <- erlang:processes(), shell:whereis_evaluator(X) =/= undefined]),
    %% io request
    PidList = [spawn(fun() -> io:format(Leader, Format, Args) end) || Leader <- LeaderList],
    %% kill it after 3 second if process block on io request
    spawn(fun() -> receive _ -> ok after 3000 -> [erlang:exit(Pid, kill) || Pid <- PidList] end end),
    ok.

%% @doc set shell prompt
-spec set_prompt() -> ok.
set_prompt() ->
    shell:catch_exception(true),
    shell:prompt_func({?MODULE, prompt_func}),
    ok.

%% @doc shell prompt_func
-spec prompt_func([{history, non_neg_integer()}]) -> string().
prompt_func([{history, N}]) ->
    io_lib:format("[~s](~s)~s ", [color:blue(atom_to_list(node())), color:cyan(N), color:green(<<">>">>)]).
    %% io_lib:format("[~s]['~s':~s]~s(~B) > ", [color:blue(element(2, file:get_cwd())), color:cyan(string:strip(atom_to_list(node()), both, $')), color:green(erlang:get_cookie()), color:magenta(self()), N]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
