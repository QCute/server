%%%-------------------------------------------------------------------
%%% @doc
%%% module beam(record fields info)
%%% @end
%%%-------------------------------------------------------------------
-module(beam).
-behavior(gen_server).
%% export function
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([read/0, read/1]).
-export([find/1, get/1]).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
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

%% @doc for e script
main(["update_include"]) ->
    %% update self all include
    update_include();
main(Env) ->
    Env.

%% @doc update all include
update_include() ->
    %% src/debug dir by default
    Name = escript:script_name(),
    Path = string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))),
    update_include(Path ++ "user_default.erl", Path, "../../include/").
update_include(FilePath, ScriptPath, IncludePath) ->
    %% list all file
    {ok, LineList} = file:list_dir_all(ScriptPath ++ IncludePath),
    %% extract file name from file path
    {_, [[Name]]} = re:run(FilePath, "\\w+(?=\\.erl)", [global, {capture, first, list}]),
    %% construct include line
    Include = ["-include(\"" ++ IncludePath ++ Line ++ "\").\n" || Line <- LineList, string:str(Line, ".hrl") =/= 0],
    IncludePatten = "(?m)(^-include.+?)(?=\\.$)\\.\n?",
    %% construct data and patten
    %% module declare
    Module = "-module(" ++ Name ++ ").\n",
    ModulePatten = "-module\\(" ++ Name ++ "\\)\\.\n",
    %% no warn declare
    NoWarn = "-compile(nowarn_export_all).\n",
    NoWarnPatten = "-compile\\(nowarn_export_all\\)\\.\n",
    %% export declare
    Export = "-compile(export_all).\n",
    ExportPatten = "-compile\\(export_all\\)\\.\n",
    %% read file data
    Data = binary_to_list(max(element(2, file:read_file(FilePath)), <<>>)),
    %% remove old data
    NewData = lists:foldr(fun(P, L) -> re:replace(L, P, "", [global, {return, list}]) end, Data, [ModulePatten, NoWarnPatten, ExportPatten, IncludePatten]),
    %% concat head include and other origin code
    file:write_file(FilePath, Module ++ NoWarn ++ Export ++ Include ++ NewData),
    ok.

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
