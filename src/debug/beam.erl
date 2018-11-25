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
-export([hot/0, reload/0, reload/3]).
-export([read/0, read/1]).
-export([find/1]).
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

%% @doc hot load record
hot() ->
    catch gen_server:cast(?MODULE, {hot}).

%% @doc for e script
main([]) ->
    reload("../../src/debug/user_default.erl", "../../include/", "../../beam/");
main([F, I, B| _]) ->
    reload(F, I, B);
main(_) ->
    io:format("escript beam.erl FilePath IncludePath BeamPath").

%% @doc reload all hrl file (record)
reload() ->
    reload("../src/debug/user_default.erl", "../include/", "../beam/").
reload(FilePath, IncludePath, BeamPath) ->
    case os:type() of
        {win32, nt} ->
            Line = "\r\n",
            ListCommand = "powershell ls ";
        _ ->
            Line = "\n",
            ListCommand = "ls -l "
    end,
    LineList = string:tokens(os:cmd(ListCommand ++ IncludePath), Line),
    Include = ["-include(\"" ++ IncludePath ++ hd(lists:reverse(string:tokens(X, " "))) ++ "\").\n" || X <- LineList, string:str(X, ".hrl") =/= 0],
    {_, [[Module]]} = re:run(FilePath, "\\w+(?=\\.erl)", [global, {capture, first, list}]),
    Head = "-module(" ++ Module ++ ").\n-compile(nowarn_export_all).\n-compile(export_all).\n",
    {_, Data} = file:read_file(FilePath),
    WithoutIncludeList = re:replace(binary_to_list(Data), "(?m)(^-include.+?)(?=\\.$)\\.\n?", "", [global, {return, list}]),
    FileData = re:replace(WithoutIncludeList, "-module\\(" ++ Module ++ "\\)\\.\n-compile\\(nowarn_export_all\\)\\.\n-compile\\(export_all\\)\\.\n", Head ++ Include, [global, {return, list}]),
    file:write_file(FilePath, FileData),
    %% recompile
    c:c(FilePath, [debug_info, {outdir, BeamPath}]),
    %% reload
    c:l(Module).

%% @doc read beam record
read() ->
    BeamName = "../beam/user_default.beam",
    read(BeamName).
read(File) ->
    case beam_lib:chunks(File, [abstract_code, "CInf"]) of
        {ok, {_Mod, [{abstract_code, {_Version, Forms}}, {"CInf", _CB}]}} ->
            %% File Chunks
            dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}, _} <- Info]]} || {attribute, _, record, {Name, Info}} <- Forms]);
        {ok, {_Mod, [{abstract_code, no_abstract_code}, {"CInf", _CB}]}} ->
            %% no abstract code (compile without debug_info)
            dict:new();
        _ ->
            %% Could be that the "Abstract" chunk is missing (pre R6).
            dict:new()
    end.
%%====================================================================
%% gen_server callback
%%====================================================================
init([]) ->
    {ok, read()}.
handle_call({find, K}, _, State) ->
    {reply, dict:find(K, State), State};
handle_call(_Info, _From, State)->
    {reply, ok, State}.
handle_cast({hot}, _State) ->
    {noreply, read()};
handle_cast(_Info, State)->
    {noreply, State}.
handle_info(_Info, State)->
    {noreply, State}.
terminate(normal, Status) ->
    {ok, Status}.
code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
