%%%-------------------------------------------------------------------
%%% @doc
%%% application runtime journal tool
%%% @end
%%%-------------------------------------------------------------------
-module(journal).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4]).
-export([print_stacktrace/5]).
-export([format_stacktrace/3]).
-export([format/1, format/2]).
-export([set_prompt/0, prompt_func/1]).
%% Includes
-include("journal.hrl").
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
-spec print_stacktrace(Module :: atom(), Line :: non_neg_integer(), Class :: atom(), Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Module, Line, Class, Reason, StackTrace) ->
    String = format_stacktrace(Class, Reason, StackTrace),
    notifier:notify(Module, Line, String),
    error_logger:error_msg(String).

%% @doc format stacktrace message
-spec format_stacktrace(Class :: atom(), Reason :: term(), Stacktrace :: term()) -> string().
format_stacktrace(Class, Reason, StackTrace) ->
    Option = maps:put(format_fun, fun(T, _) -> io_lib:format("~tp", [T]) end, maps:new()),
    erl_error:format_exception(Class, Reason, StackTrace, Option).

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
