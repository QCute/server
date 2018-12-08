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
-export([find/1]).
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

%% @doc read beam record
read() ->
    BeamName = "../beam/user_default.beam",
    read(BeamName).
read(File) ->
    case beam_lib:chunks(File, [abstract_code, "CInf"]) of
        {ok, {_Mod, [{abstract_code, {_Version, Forms}}, {"CInf", _CB}]}} ->
            %% File Chunks
            Dict = dict:from_list([{Name, [Name | [Field || {record_field, _, {_, _ , Field}, _} <- Info]]} || {attribute, _, record, {Name, Info}} <- Forms]),
            {ok, Dict};
        {ok, {_Mod, [{abstract_code, no_abstract_code}, {"CInf", _CB}]}} ->
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
handle_call(_Info, _From, State)->
    {reply, ok, State}.
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
