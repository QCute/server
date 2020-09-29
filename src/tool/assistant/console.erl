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
%% Macros
-ifdef(DEBUG).
-define(IO(F), io:format(F)).
-define(IO(F, A), io:format(F, A)).
-define(WARN(String), color:red(String)).
-define(BRIGHT(String), color:cyan(String)).
-define(ARROW, lists:flatten(color:green("➡"))).
-else.
-define(IO(F), error_logger:error_msg(F)).
-define(IO(F, A), error_logger:error_msg(F, A)).
-define(WARN(String), String).
-define(BRIGHT(String), String).
-define(ARROW, "➡").
-endif.
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
    ?IO(FormatList, Args).

%% @doc print formatted stacktrace message
-spec print_stacktrace(Stacktrace :: term()) -> ok | term().
print_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    ?IO(format_stacktrace(Reason, StackTrace));
print_stacktrace(Other) ->
    ?IO(Other).

%% @doc print formatted stacktrace message
-spec print_stacktrace(Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Reason, StackTrace) ->
    ?IO(format_stacktrace(Reason, StackTrace)).

%% @doc print formatted stacktrace message
-spec format_stacktrace(Stacktrace :: term()) -> string().
format_stacktrace({'EXIT', {Reason, StackTrace}}) ->
    format_stacktrace(Reason, StackTrace);
format_stacktrace(Other) ->
    io_lib:format("~1024p~n", [Other]).

%% @doc format stacktrace message
-spec format_stacktrace(Reason :: term(), Stacktrace :: term()) -> string().
format_stacktrace(Reason, StackTrace) ->
    %% format exception reason
    ReasonMsg = format_reason(Reason, StackTrace),
    %% format exception stacktrace
    StackMsg = [io_lib:format("➡   ~s:~s(~s:~w)~n", [Module, Function, FileName, Line]) || {Module, Function, _ArityOrArgs, [{file, FileName}, {line, Line}]} <- StackTrace],
    %% format exception msg to tty/file
    io_lib:format("~ts~ts", [ReasonMsg, StackMsg]).

%% format exception reason
format_reason({pool_error, {PoolId, Reason}}, _) ->
    io_lib:format("~ncatch exception: ~w(PoolId): ~w~n    ~w~n", [pool_error, PoolId, Reason]);
format_reason({mysql_error, {Sql, ErrorCode, Reason}}, _) ->
    io_lib:format("~ncatch exception: ~w~nErrorCode: ~w~nsql: ~s~nreason: ~s~n", [mysql_error, ErrorCode, Sql, Reason]);
format_reason({badmatch, Match}, _) ->
    io_lib:format("~ncatch exception: ~w ➡ ~w~n", [badmatch, Match]);
format_reason({case_clause, Match}, _) ->
    io_lib:format("~ncatch exception: ~w ➡ ~w~n", [case_clause, Match]);
format_reason(function_clause, [{Module, Function, Args, _} | _]) ->
    AF = string:join(lists:duplicate(length(Args), "~1024p"), ", "),
    io_lib:format("~ncatch exception: ~w ➡ ~w:~w(" ++ AF ++ ")~n", [function_clause, Module, Function | Args]);
format_reason(badarg, [{Module, Function, Args, _} | _]) ->
    AF = string:join(lists:duplicate(length(Args), "~1024p"), ", "),
    io_lib:format("~ncatch exception: ~w ➡ ~w:~w(" ++ AF ++ ")~n", [badarg, Module, Function | Args]);
format_reason(undef, [{Module, Function, Args, _} | _]) ->
    AF = string:join(lists:duplicate(length(Args), "~1024p"), ", "),
    io_lib:format("~ncatch exception: ~w ➡ ~w:~w(" ++ AF ++ ")~n", [undef, Module, Function | Args]);
format_reason({noproc, {Module, Function, Args}}, _) ->
    AF = string:join(lists:duplicate(length(Args), "~w"), ", "),
    io_lib:format("~ncatch exception: ~w ➡ ~w:~w(" ++ AF ++ ")~n", [noproc, Module, Function | Args]);
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
    spawn(fun() -> receive _ -> ok after 3000 -> [exit(Pid, kill) || Pid <- PidList] end end),
    ok.

%% @doc set shell prompt
-spec set_prompt() -> 'default' | {module(), atom()}.
set_prompt() ->
    shell:prompt_func({?MODULE, prompt_func}).

%% @doc shell prompt_func
-spec prompt_func([{history, non_neg_integer()}]) -> string().
prompt_func([{history, N}]) ->
    io_lib:format("['~s':~s]~s(~B) > ", [color:cyan(string:strip(atom_to_list(node()), both, $')), color:green(erlang:get_cookie()), color:magenta(self()), N]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
