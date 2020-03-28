%%%------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% API
-export([load/3, load/2]).
-export([object/1, source/1, version/1, md5/1]).
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
    ChecksumList = [{Module, md5(Module)} || Module <- Modules],
    [io:format("node:~1024p result:~1024p~n", [Node, rpc:call(Node, beam, load, [ChecksumList, Mode], 1000)]) || Node <- Nodes].

%% @doc soft/purge and load module (remote call)
-spec load([{atom(), binary()}], atom()) -> [{module(), boolean(), {ok, module()} | {error, code:load_error_rsn()} | {skip, unloaded}, boolean()}].
load(Modules, Mode) ->
    load_loop(Modules, Mode, []).

load_loop([], _, Result) ->
    lists:reverse(Result);
load_loop([{Module, Digest} | T], load, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, load, [{Module, false, {skip, unloaded}, false} | Result]);
        _ ->
            %% use md5 digest verify module instead version avoid version empty when strip beam file
            load_loop(T, load, [{Module, code:soft_purge(Module), code:load_file(Module), md5(Module) == Digest} | Result])
    end;
load_loop([{Module, Digest} | T], force, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, force, [{Module, false, {skip, unloaded}, false} | Result]);
        _ ->
            %% use md5 digest verify module instead version avoid version empty when strip beam file
            load_loop(T, force, [{Module, code:purge(Module), code:load_file(Module), md5(Module) == Digest} | Result])
    end.

%% @doc beam object file
-spec object(Module :: module()) -> string().
object(Module) ->
    %% locate beam file, do not load module in memory
    case code:which(Module) of
        File when is_list(File) ->
            File;
        _ ->
            []
    end.

%% @doc beam source file
-spec source(Module :: module()) -> string().
source(Module) ->
    %% can use Module:module_info(compile) => [..., {source, File}, ...], but it will load module in memory
    case beam_lib:chunks(object(Module), [compile_info]) of
        {ok, {_, [{compile_info, CompileInfo}]}} ->
            proplists:get_value(source, CompileInfo, []);
        _ ->
            []
    end.

%% @doc beam version
-spec version(Module :: module()) -> list().
version(Module) ->
    %% can use Module:module_info(attribute) => [..., {vsn, Version}, ...], but it will load module in memory
    case beam_lib:version(object(Module)) of
        {ok, {_, Version}} ->
            Version;
        _ ->
            []
    end.

%% @doc beam md5 digest
-spec md5(Module :: module()) -> binary().
md5(Module) ->
    %% it can replace with code:module_md5
    %% OTP 18 or later, get digest can use Module:module_info(md5), but it will load module in memory
    case beam_lib:md5(object(Module)) of
        {ok, {_, Digest}} ->
            Digest;
        _ ->
            <<>>
    end.

%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
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
    Forms = lists:append([element(2, epp:parse_file(File, [], [])) ||  File <- filelib:wildcard(config:path_include() ++ "*.hrl")]),
    %% extract record field name
    [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms].

%% @doc read beam record with file
-spec read(File :: file:filename()) -> list().
read(File) ->
    case beam_lib:chunks(File, [abstract_code, compile_info]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}, {compile_info, _CompileInfo}]}} ->
            %% File Chunks
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            %% Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}, _} <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms]),
            [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms];
        {ok, {_Module, [{abstract_code, {_Version, Forms}}, {compile_info, CompileInfo}]}} ->
            case [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms] of
                [] ->
                    %% {"CInf", ChunkBlock}
                    %% ChunkBlock = binary_to_term(CB),
                    Options = proplists:get_value(options, CompileInfo, []),
                    Source = proplists:get_value(source, CompileInfo),
                    IncludePath = [filename:dirname(File) | [P || {i, P} <- Options, is_list(P)]],
                    PreDefine = [case X of {d, M, V} -> {M, V}; {d, M} -> M end || X <- Options, element(1, X) == d],
                    {ok, FileForms} = epp:parse_file(Source, IncludePath, PreDefine),
                    [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- FileForms];
                List ->
                    List
            end;
        {ok, {_Module, [{abstract_code, no_abstract_code}, {compile_info, CompileInfo}]}} ->
            %% no abstract code (compile without debug_info)
            %% {"CInf", ChunkBlock}
            %% ChunkBlock = binary_to_term(CB),
            Options = proplists:get_value(options, CompileInfo, []),
            Source = proplists:get_value(source, CompileInfo),
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
    erlang:process_flag(trap_exit, true),
    case read() of
        [] ->
            %% user_default beam file abstract code
            {ok, read(object(user_default))};
        Records ->
            %% all include file abstract code
            {ok, Records}
    end.

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
