%%%------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
-compile({no_auto_import, [get/1]}).
%% API
-export([load/3, load/2]).
-export([checksum/1]).
-export([field/3]).
-export([find/1, get/1]).
-export([read/0, read/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load modules on nodes
-spec load(Nodes :: [atom()], Modules :: [atom()], Mode :: atom()) -> term().
load(Nodes, Modules, Mode) ->
    ChecksumList = [{Module, beam:checksum(Module)} || Module <- Modules],
    [io:format("node:~p result:~p~n", [Node, rpc:call(Node, beam, load, [ChecksumList, Mode], 1000)]) || Node <- Nodes].

%% @doc soft/purge and load module (remote call)
-spec load([{atom(), list()}], atom()) -> ok.
load(Modules, Mode) ->
    load_loop(Modules, Mode, []).

load_loop([], _, Result) ->
    lists:reverse(Result);
load_loop([{Module, Vsn} | T], load, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, load, [{Module, false, {error, unloaded}, false} | Result]);
        _ ->
            Purge = code:soft_purge(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_loop(T, load, [{Module, Purge, Load, Checksum == Vsn} | Result])
    end;
load_loop([{Module, Vsn} | T], force, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, force, [{Module, false, {error, unloaded}, false} | Result]);
        _ ->
            Purge = code:purge(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_loop(T, force, [{Module, Purge, Load, Checksum == Vsn} | Result])
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

%% @doc get record field data
-spec field(Record :: tuple(), Tag :: atom(), Field :: atom()) -> term().
field(Record, Tag, Field) ->
    FieldList = get(Tag),
    N = listing:index(Field, FieldList),
    erlang:element(N, Record).

%% @doc find record
-spec find(atom()) -> list().
find(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {find, K}).

%% @doc get record
-spec get(atom()) -> list() | error.
get(K) ->
    catch start_link(),
    catch gen_server:call(?MODULE, {get, K}).

%% @doc read beam record
-spec read() -> {ok, dict:dict()} | {error, term()}.
read() ->
    BeamName = config:path_beam() ++ "/user_default.beam",
    read(BeamName).

%% @doc read beam record with file
-spec read(File :: file:filename()) -> {ok, dict:dict()} | {error, term()}.
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
%%%==================================================================
%%% gen_server callback
%%%==================================================================
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
%%%==================================================================
%%% Internal functions
%%%==================================================================
