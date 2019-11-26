%%%------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% API
-export([load/3, load/2]).
-export([source/1, object/1, checksum/1, digest/1]).
-export([field/2]).
-export([find/1]).
-export([read/0, read/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load modules on nodes
-spec load(Nodes :: [atom()], Modules :: [module()], Mode :: atom()) -> term().
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
            load_loop(T, load, [{Module, false, {skip, unloaded}, false} | Result]);
        _ ->
            Purge = code:soft_purge(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_loop(T, load, [{Module, Purge, Load, Checksum == Vsn} | Result])
    end;
load_loop([{Module, Vsn} | T], force, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, force, [{Module, false, {skip, unloaded}, false} | Result]);
        _ ->
            Purge = code:purge(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_loop(T, force, [{Module, Purge, Load, Checksum == Vsn} | Result])
    end.

%% @doc beam source file
-spec source(Module :: module()) -> string().
source(Module) ->
    case catch Module:module_info(compile) of
        {'EXIT', _} ->
            [];
        Attributes ->
            proplists:get_value(source, Attributes, [])
    end.

%% @doc beam object file
-spec object(Module :: module()) -> string().
object(Module) ->
    case code:is_loaded(Module) of
        {file, File} ->
            File;
        _ ->
            []
    end.

%% @doc beam checksum
-spec checksum(Module :: module()) -> list().
checksum(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} ->
            [];
        Attributes ->
            proplists:get_value(vsn, Attributes, [])
    end.

%% @doc beam md5 digest
-spec digest(Module :: module()) -> binary().
digest(Module) ->
    case file:read_file(object(Module)) of
        {ok, Binary} ->
            code:module_md5(Binary);
        _ ->
            <<>>
    end.

%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc get record field data
-spec field(Record :: tuple(), Field :: atom()) -> term().
field(Record, Field) ->
    FieldList = find(element(1, Record)),
    N = listing:index(Field, FieldList),
    erlang:element(N, Record).

%% @doc find record
-spec find(atom()) -> list().
find(Tag) ->
    start_link(),
    gen_server:call(?MODULE, {find, Tag}).

%% @doc read beam record
-spec read() -> list().
read() ->
    %% read only include file record info
    Forms = lists:append([element(2, epp:parse_file(config:path_include() ++ File, [], [])) ||  File <- element(2, file:list_dir(config:path_include()))]),
    %% extract record field name
    [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms].

%% @doc read beam record with file
-spec read(File :: file:filename()) -> list().
read(File) ->
    case beam_lib:chunks(File, [abstract_code, "CInf"]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}, {"CInf", _CB}]}} ->
            %% File Chunks
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}, _} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms];
        {ok, {_Module, [{abstract_code, {_Version, Forms}}, {"CInf", CB}]}} ->
            case [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms] of
                [] ->
                    ChunkBlock = binary_to_term(CB),
                    Options = proplists:get_value(options, ChunkBlock, []),
                    Source = proplists:get_value(source, ChunkBlock),
                    IncludePath = [filename:dirname(File) | [P || {i, P} <- Options, is_list(P)]],
                    PreDefine = [case X of {d, M, V} -> {M, V}; {d, M} -> M end || X <- Options, element(1, X) == d],
                    {ok, FileForms} = epp:parse_file(Source, IncludePath, PreDefine),
                    [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- FileForms];
                List ->
                    List
            end;
        {ok, {_Module, [{abstract_code, no_abstract_code}, {"CInf", CB}]}} ->
            %% no abstract code (compile without debug_info)
            ChunkBlock = binary_to_term(CB),
            Options = proplists:get_value(options, ChunkBlock, []),
            Source = proplists:get_value(source, ChunkBlock),
            IncludePath = [filename:dirname(File) | [P || {i, P} <- Options, is_list(P)]],
            PreDefine = [case X of {d, M, V} -> {M, V}; {d, M} -> M end || X <- Options, element(1, X) == d],
            {ok, FileForms} = epp:parse_file(Source, IncludePath, PreDefine),
            [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- FileForms];
        {error, beam_lib, _Reason} ->
            %% Could be that the "Abstract" chunk is missing (pre R6).
            []
    end.

%%%==================================================================
%%% gen_server callback
%%%==================================================================
init([]) ->
    {ok, read()}.

handle_call({find, Tag}, _, State) ->
    {reply, element(2, listing:key_find(Tag, 1, State, {Tag, []})), State};

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
