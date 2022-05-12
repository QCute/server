%%%-------------------------------------------------------------------
%%% @doc
%%% beam (record fields info) tool
%%% @end
%%%-------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% API
-export([load/3, load/2]).
-export([object/1, source/1, version/1, loaded_version/1, md5/1, loaded_md5/1]).
-export([diff/2, diff/1]).
-export([compress/2, compress/3, extract/1]).
-export([find/1, field/2]).
-export([read_from_include/0, read_from_ets/0, read_from_beam/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("time.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load modules on nodes
-spec load(Nodes :: [node()], Modules :: [module()], Mode :: load | force) -> ok.
load(Nodes, Modules, Mode) ->
    ChecksumList = [{Module, md5(Module)} || Module <- Modules],
    lists:foreach(fun(Node) ->
        case rpc:call(Node, ?MODULE, ?FUNCTION_NAME, [ChecksumList, Mode], ?CALL_TIMEOUT) of
            {badrpc, Reason} ->
                io:format("Node:~p Reason: ~p~n~n", [Node, Reason]);
            List ->
                io:format("Node:~p~n", [Node]),
                journal:print_row_table(List),
                io:format("~n")
        end
    end, Nodes).

%% @doc soft/purge and load module (remote call)
-spec load([{atom(), binary()}], atom()) -> [{module(), boolean(), boolean(), {ok, module()} | {error, code:load_error_rsn()} | {skip, unloaded}}].
load(Modules, Mode) ->
    load_loop(Modules, Mode, []).

load_loop([], _, Result) ->
    [{module, digest, purge, load} | lists:reverse(Result)];
load_loop([{Module, Digest} | T], load, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, load, [{Module, false, false, {skip, unloaded}} | Result]);
        _ ->
            %% use md5 digest verify module instead version avoid version empty when strip beam file
            load_loop(T, load, [{Module, loaded_md5(Module) =/= Digest, code:soft_purge(Module), code:load_file(Module)} | Result])
    end;
load_loop([{Module, Digest} | T], force, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_loop(T, force, [{Module, false, false, {skip, unloaded}} | Result]);
        _ ->
            %% use md5 digest verify module instead version avoid version empty when strip beam file
            load_loop(T, force, [{Module, loaded_md5(Module) =/= Digest, code:purge(Module), code:load_file(Module)} | Result])
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

%% @doc loaded beam version
-spec loaded_version(Module :: module()) -> list().
loaded_version(Module) ->
    %% can use Module:module_info(attribute) => [..., {vsn, Version}, ...], but it will load module in memory
    case code:is_loaded(Module) =/= false andalso proplists:get_value(vsn, erlang:get_module_info(Module, attributes), []) of
        false ->
            [];
        Version ->
            Version
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

%% @doc loaded beam md5 digest
-spec loaded_md5(Module :: module()) -> binary().
loaded_md5(Module) ->
    %% it can replace with code:module_md5
    %% OTP 18 or later, get digest can use Module:module_info(md5), but it will load module in memory
    case code:is_loaded(Module) =/= false andalso erlang:get_module_info(Module, md5) of
        false ->
            <<>>;
        Digest ->
            Digest
    end.

%% @doc load modules on nodes
-spec diff(Nodes :: [node()], Modes :: [skip | true | false]) -> ok.
diff(Nodes, Modes) ->
    lists:foreach(fun(Node) ->
        case rpc:call(Node, ?MODULE, ?FUNCTION_NAME, [Modes], ?CALL_TIMEOUT) of
            {badrpc, Reason} ->
                io:format("Node:~p Reason: ~p~n~n", [Node, Reason]);
            List ->
                io:format("Node:~p~n", [Node]),
                journal:print_column_table(List),
                io:format("~n")
        end
    end, Nodes).

%% @doc beam diff
-spec diff(Modes :: [skip | true | false]) -> [[atom()]].
diff(Modes) ->
    Files = filelib:wildcard(lists:concat([config:path_beam(), "/*.beam"])),
    diff_loop(Files, Modes, [], [], []).

diff_loop([], [], Skip, True, False) ->
    [[skip | lists:reverse(Skip)], [true | lists:reverse(True)], [false | lists:reverse(False)]];
diff_loop([], Modes, Skip, True, False) ->
    SkipList = [[skip | lists:reverse(Skip)] || lists:member(skip, Modes)],
    TrueList = [[true | lists:reverse(True)] || lists:member(true, Modes)],
    FalseList = [[false | lists:reverse(False)] || lists:member(false, Modes)],
    lists:append(SkipList, lists:append(TrueList, FalseList));
diff_loop([File | T], Modes, Skip, True, False) ->
    Module = list_to_atom(filename:basename(File, ".beam")),
    case code:is_loaded(Module) of
        false ->
            diff_loop(T, Modes, [Module | Skip], True, False);
        _ ->
            case md5(Module) == loaded_md5(Module) of
                true ->
                    diff_loop(T, Modes, Skip, [Module | True], False);
                false ->
                    diff_loop(T, Modes, Skip, True, [Module | False])
            end
    end.

%% @doc compress beams to escript
-spec compress(Name :: file:filename(), Pattern :: file:filename()) -> ok | {error, file:posix() | badarg | terminated | system_limit}.
compress(Name, Pattern) ->
    compress(Name, Pattern, <<>>).

%% @doc compress beams to escript
-spec compress(Name :: file:filename(), Pattern :: file:filename(), Shebang :: iolist()) -> ok | {error, file:posix() | badarg | terminated | system_limit}.
compress(Name, Pattern, Shebang) ->
    FileList = [{filename:basename(File), element(2, file:read_file(File))} || File <- filelib:wildcard(Pattern)],
    {ok, {_Name, Binary}} = zip:zip("achieve", FileList, [memory]),
    file:write_file(Name, <<"#!/usr/bin/env escript\n", (iolist_to_binary(Shebang))/binary, Binary/binary>>).

%% @doc extract and load compressed escript beams
-spec extract(File :: filelib:filename()) -> code:load_ret().
extract(File) ->
    {ok, Binary} = escript:parse_file(File),
    {ok, FileInfo} = file:read_file_info(File),
    code:set_primary_archive(File, Binary, FileInfo, fun escript:parse_file/1),
    code:load_file(list_to_atom(filename:basename(File))).

%% @doc find record
-spec find(atom()) -> list().
find(Tag) ->
    start_link(),
    gen_server:call(?MODULE, {find, Tag}).

%% @doc get record field data
-spec field(Record :: tuple(), Field :: atom()) -> term().
field(Record, Field) ->
    FieldList = find(element(1, Record)),
    N = listing:index(Field, FieldList),
    element(N, Record).

%% @doc read beam record
-spec read_from_include() -> list().
read_from_include() ->
    %% read only include file record info
    Forms = lists:append([element(2, epp:parse_file(File, [], [])) || File <- filelib:wildcard(lists:concat([config:path_include(), "*.hrl"]))]),
    %% extract record field name
    [{Name, [Name | [element(3, element(3, Field)) || Field <- FieldList]]} || {attribute, _, record, {Name, FieldList}} <- Forms].

%% @doc read beam record
-spec read_from_ets() -> list().
read_from_ets() ->
    Tab = hd([Tab || Tab <- ets:all(), ets:info(Tab, name) == shell_records]),
    List = ets:select(Tab, ets:fun2ms(fun({_, {attribute, _, record, {Tag, Fields}}}) -> {Tag, Fields} end)),
    [{Tag, [Tag | [Name || {record_field, _, {_, _, Name}, _} <- Fields]]} || {Tag, Fields} <- List].

%% @doc read beam record with file
-spec read_from_beam(File :: file:filename()) -> list().
read_from_beam(File) ->
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

%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callback
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, list()}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    case read_from_include() of
        [] ->
            %% user_default beam file abstract code
            {ok, read_from_ets()};
        Records ->
            %% all include file abstract code
            {ok, Records}
    end.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: list()) -> {reply, Reply :: term(), NewState :: list()}.
handle_call({find, Tag}, _, State) ->
    {reply, element(2, listing:key_find(Tag, 1, State, {Tag, []})), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: list()) -> {ok, NewState :: list()}.
terminate(normal, Status) ->
    {ok, Status}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: list(), Extra :: term()) -> {ok, NewState :: list()}.
code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
