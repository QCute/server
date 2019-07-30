%%%-------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%-------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% API
-export([load/1, load/2]).
-export([force_load/1, force_load/2]).
-export([load/3, load_callback/4]).
-export([locate/2]).
-export([checksum/1]).
-export([find/1, get/1]).
-export([read/0, read/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load module for all node, shell execute compatible
-spec load(atom() | [atom()]) -> ok.
load(Modules) ->
    load(all_nodes(), Modules).

%% @doc force load module for all node, shell execute compatible
-spec force_load(atom() | [atom()]) -> ok.
force_load(Modules) ->
    force_load(all_nodes(), Modules).

%% @doc load module (local call)
-spec load(atom() | [atom()], atom() | [atom()]) -> ok.
load(Nodes, Modules) ->
    load(soft, Nodes, Modules).

%% @doc force load module (local call)
-spec force_load(atom() | [atom()], atom() | [atom()]) -> ok.
force_load(Nodes, Modules) ->
    load(force, Nodes, Modules).

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
    Result = load_callback_loop(Modules, Mode, []),
    erlang:send(Pid, {Ref, lists:reverse(Result)}),
    ok.

load_callback_loop([], _, Result) ->
    Result;
load_callback_loop([{Module, Vsn} | T], soft, Result) ->
    Purge = code:soft_purge(Module),
    Load = code:load_file(Module),
    Checksum = checksum(Module),
    load_callback_loop(T, soft, [{node(), Module, Purge, Load, Checksum == Vsn} | Result]);
load_callback_loop([{Module, Vsn} | T], force, Result) ->
    Purge = code:purge(Module),
    Load = code:load_file(Module),
    Checksum = checksum(Module),
    load_callback_loop(T, force, [{node(), Module, Purge, Load, Checksum == Vsn} | Result]);
load_callback_loop([{Module, Vsn} | T], Mode, Result) ->
    file:set_cwd(io_lib:format("script/~s/", [Mode])),
    {ok, [{_, Option}]} = file:consult("Emakefile"),
    [File | _] = hd(locate("../../src", atom_to_list(Module) ++ ".erl")),
    Load = c:c(File, Option),
    Checksum = checksum(Module),
    file:set_cwd("../../"),
    load_callback_loop(T, Mode, [{node(), Module, true, Load, Checksum == Vsn} | Result]).

%% @doc find module file from source path
-spec locate(Path :: string(), File :: file:filename()) -> [file:filename()].
locate(Path, File) ->
    {ok, FileList} = file:list_dir_all(Path),
    locate_loop(FileList, Path, File, []).

%% depth first search
locate_loop([], _, _, List) ->
    List;
locate_loop([Name | T], Path, File, List) ->
    SubFile = Path ++ "/" ++ Name,
    case filelib:is_dir(SubFile) of
        true ->
            %% sub dir recursion
            Result = locate(SubFile, File),
            locate_loop(T, Path, File, List ++ Result);
        false when Name =:= File ->
            locate_loop(T, Path, File, [SubFile | List]);
        _ ->
            locate_loop(T, Path, File, List)
    end.

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
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc find record
-spec find(atom()) -> list().
find(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {find, K}).

%% @doc get record
-spec get(atom()) -> list() | 'error'.
get(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {get, K}).

%% @doc read beam record
read() ->
    BeamName = config:path_beam() ++ "/user_default.beam",
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
        {ok, Value} ->
            Result = Value;
        _ ->
            Result = []
    end,
    {reply, Result, State};
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
    case init:get_argument('BEAM_LOADER_NODES') of
        error ->
            %% given by data configure
            All = node_data:all(),
            IP = hd(tl(string:tokens(atom_to_list(node()), "@"))),
            %% chose local ip when ip not set
            [list_to_atom(lists:concat([Node, "@", tool:default(node_data:ip(Node), IP)])) || Node <- All];
        {ok, [NodeList]} ->
            %% given by shell
            [list_to_atom(Node) || Node <- NodeList]
    end.

%% handle remote result
handle_result([]) ->
    io:format("~n~n");
handle_result([{Node, Module, _, {error, Error}, _} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, Error]),
    handle_result(T);
handle_result([{Node, Module, _, {_, Module}, true} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, true]),
    handle_result(T).
