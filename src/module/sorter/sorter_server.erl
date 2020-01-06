%%%------------------------------------------------------------------
%%% @doc
%%% module sorter server
%%% @end
%%%------------------------------------------------------------------
-module(sorter_server).
-behaviour(gen_server).
%% API
-export([start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("sorter.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start_link(Name :: atom(), Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([Name, share, Type, Limit, Key, Value, Time, Rank, Data]) ->
    %% make new sorter
    Sorter = sorter:new(Name, share, Type, Limit, Key, Value, Time, Rank, Data),
    {ok, Sorter}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({update, Data}, Sorter = #sorter{}) ->
    try
        sorter:update(Data, Sorter)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, Sorter};
handle_cast(stop, State) ->
    try
        sorter:drop(State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {stop, normal, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
