%%%-------------------------------------------------------------------
%%% @doc
%%% console print tool
%%% @end
%%%-------------------------------------------------------------------
-module(console).
%% API
-export([print/4, debug/4, info/4, warming/4, error/4]).
-export([print_stacktrace/1, print_stacktrace/4]).
-export([format_stacktrace/1, format_stacktrace/2]).
-export([format/1, format/2]).
-export([set_prompt/0, prompt_func/1]).
-export([start/0, start_link/0, clean_notify/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
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
-spec print_stacktrace(Module :: atom(), Line :: non_neg_integer(), Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Module, Line, Reason, StackTrace) ->
    String = format_stacktrace(Reason, StackTrace),
    notify(Module, Line, String),
    error_logger:error_msg(String).

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
    io_lib:format("~ncatch exception: ~w~n", [Reason]).

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
%%% Exception Notify
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc start link
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc notify
-spec notify(Module :: atom(), Line :: non_neg_integer(), Reason :: term()) -> ok.
notify(Module, Line, Reason) ->
    gen_server:cast(?MODULE, {notify, Module, Line, Reason}).

%% @doc clean alert
-spec clean_notify() -> ok.
clean_notify() ->
    gen_server:call(?MODULE, clean_notify).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    inets:start(),
    ssl:start(),
    erlang:process_flag(trap_exit, true),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: list()) -> {reply, Reply :: term(), NewState :: list()}.
handle_call(clean_notify, _From, _State) ->
    {reply, ok, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_cast({notify, Module, Line, Reason}, State) ->
    try
        case lists:member({Module, Line}, State) of
            true ->
                {noreply, State};
            false ->
                %% read notify key file config in real time
                {ok, [[Home | _] | _]} = init:get_argument(home),
                File = lists:concat([Home, "/.notify/config"]),
                filelib:ensure_dir(File),
                not filelib:is_file(File) andalso file:write_file(File, <<>>),
                {ok, Data} = file:read_file(File),
                %% notify
                Title = encoding:url_encode(lists:concat(["Server (Id: " , config:server_id(), ") Catch Exception!"])),
                Content = encoding:url_encode(lists:flatten(string:replace(lists:flatten(Reason), "\n", "\r\n", all))),
                %% go to https://xizhi.qqoq.net/ get the sec key
                F = fun(Key) -> httpc:request(lists:concat(["https://xizhi.qqoq.net/", Key, ".send?title=", Title, "&content=", Content])) end,
                lists:foreach(F, string:tokens(binary_to_list(binary:replace(Data, <<"\r">>, <<>>, [global])), "\n")),
                {noreply, [{Module, Line} | State]}
        end
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        error_logger:error_msg(format_stacktrace(Reason, ?GET_STACKTRACE(Stacktrace)))
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: list()) -> {ok, NewState :: list()}.
terminate(_, State) ->
    %% receiver closed
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: list(), Extra :: term()) -> {ok, NewState :: list()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
