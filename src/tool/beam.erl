%%%-------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%-------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% export function
-export([load/1, load/2, load/3, load_callback/4, checksum/1]).
-export([find/1, get/1]).
-export([read/0, read/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load module for all node, shell execute compatible
-spec load(atom() | [atom()]) -> ok.
load(Modules) ->
    load(all_nodes(), Modules).

%% @doc load module (local call)
-spec load(atom() | [atom()], atom() | [atom()]) -> ok.
load(Nodes, Modules) ->
    load(soft, Nodes, Modules).

%% @doc load module (local call)
-spec load(atom(), atom() | [atom()], atom() | [atom()]) -> ok.
load(Mode, Node, Modules) when is_atom(Node) ->
    load(Mode, [Node], Modules);
load(Mode, Nodes, Module) when is_atom(Module) ->
    load(Mode, Nodes, [Module]);
load(Mode, Node, Module) when is_atom(Node) andalso is_atom(Module) ->
    load(Mode, [Node], [Module]);
load(Mode, Nodes, Modules) ->
    Ref = make_ref(),
    ChecksumModules = [{Module, checksum(Module)} || Module <- Modules],
    List = [{Node, net_adm:ping(Node) == pong andalso rpc:cast(Node, ?MODULE, load_callback, [Mode, self(), Ref, ChecksumModules])} || Node <- Nodes],
    [receive {Ref, Result} -> handle_result(Result) after 10 * 1000 -> io:format(standard_error, "receive timeout from ~p~n", [Node]) end || {Node, true} <- List],
    [io:format(standard_error, "cannot connect to node:~p~n", [Node]) || {Node, false} <- List],
    ok.

%% @doc soft purge and load module (remote call)
-spec load_callback(atom(), pid(), reference(), [atom()]) -> ok.
load_callback(Mode, Pid, Ref, Modules) ->
    Result = load_loop(Modules, Mode, []),
    erlang:send(Pid, {Ref, lists:reverse(Result)}),
    ok.

load_loop([], _, Result) ->
    Result;
load_loop([{Module, Vsn} | T], compile, Result) ->
    Load = c:c(Module),
    Checksum = checksum(Module),
    load_loop(T, compile, [{node(), Module, true, Load, Checksum == Vsn} | Result]);
load_loop([{Module, Vsn} | T], soft, Result) ->
    Purge = code:soft_purge(Module),
    Load = code:load_file(Module),
    Checksum = checksum(Module),
    load_loop(T, soft, [{node(), Module, Purge, Load, Checksum == Vsn} | Result]);
load_loop([{Module, Vsn} | T], Mode, Result) ->
    Purge = code:purge(Module),
    Load = code:load_file(Module),
    Checksum = checksum(Module),
    load_loop(T, Mode, [{node(), Module, Purge, Load, Checksum == Vsn} | Result]).

%% @doc beam checksum
-spec checksum(atom()) -> list().
checksum(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} ->
            [];
        Attributes ->
            proplists:get_value(vsn, Attributes, [])
    end.

%% @doc start
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc find record
find(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {find, K}).

get(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {get, K}).

%% @doc read beam record
read() ->
    BeamName = "beam/user_default.beam",
    read(BeamName).
read(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {_Version, Forms}}]}} ->
            %% File Chunks
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}, _} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            Dict = dict:from_list([{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            {ok, Dict};
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            %% no abstract code (compile without debug_info)
            {error, no_abstract_code};
        _ ->
            %% Could be that the "Abstract" chunk is missing (pre R6).
            {error, no_abstract_code}
    end.
%%====================================================================
%% gen_server callback
%%====================================================================
init([]) ->
    read().
handle_call({find, K}, _, State) ->
    {reply, dict:find(K, State), State};
handle_call({get, K}, _, State) ->
    case dict:find(K, State) of
        {ok, Result} ->
            {reply, Result, State};
        _ ->
            {reply, [], State}
    end;
handle_call(_Info, _From, State) ->
    {reply, ok, State}.
handle_cast(_Info, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(normal, Status) ->
    {ok, Status}.
code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
all_nodes() ->
    case os:getenv("BEAM_LOADER_NODE") of
        Env when Env == false orelse Env == "." ->
            All = data_node:all(),
            [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
            F = fun(Node) -> list_to_atom(lists:concat([Node, "@", ip(Node, IP)])) end,
            [F(Node) || Node <- All];
        Node ->
            [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
            [list_to_atom(lists:concat([Node, "@", ip(Node, IP)]))]
    end.
%% chose local ip when ip not set
ip(Node, LocalIP) ->
    case data_node:ip(Node) of
        [] ->
            LocalIP;
        IP ->
            IP
    end.

%% handle remote result
handle_result([]) ->
    io:format("~n~n");
handle_result([{Node, Module, _, {error, Error}, _} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, Error]),
    handle_result(T);
handle_result([{Node, Module, true, {_, Module}, true} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, true]),
    handle_result(T).
