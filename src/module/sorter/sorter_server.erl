%%%-------------------------------------------------------------------
%%% @doc
%%% module sorter server
%%% @end
%%%-------------------------------------------------------------------
-module(sorter_server).
-behaviour(gen_server).
%% export API function
-export([start/2, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("sorter.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start(Name, Args) ->
    FullName = type:to_atom(lists:concat([?MODULE, "_", Name])),
    process:start(FullName, Args).

%% @doc server start
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, share, Type, Limit, Key, Value, Time, Rank, Data]) ->
    %% make new sorter
    Sorter = sorter:new(Name, share, Type, Limit, Key, Value, Time, Rank, Data),
    {ok, Sorter};
init(_) ->
    {ok, []}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({'update', Data}, Sorter = #sorter{}) ->
    sorter:update(Data, Sorter),
    {noreply, Sorter};
handle_info('stop', State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
