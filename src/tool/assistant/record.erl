%%%-------------------------------------------------------------------
%%% @doc
%%% beam (record fields info) tool
%%% @end
%%%-------------------------------------------------------------------
-module(record).
-behavior(gen_server).
%% API
-export([find/1, field/2]).
-export([read_from_include/0, read_from_ets/0, read_from_beam/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include_lib("stdlib/include/ms_transform.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
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
