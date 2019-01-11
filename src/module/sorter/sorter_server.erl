%%%-------------------------------------------------------------------
%%% @doc
%%% module sorter server
%%% @end
%%%-------------------------------------------------------------------
-module(sorter_server).
-behaviour(gen_server).
%% export API function
-export([start/0, start/2, start_link/0, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(state, {sorter, tick}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start() ->
    process:start(?MODULE).
start(Name, Args) ->
    FullName = type:to_atom(lists:concat([?MODULE, "_", Name])),
    ChildSpec = {FullName, {?MODULE, start_link, [FullName, Args]}, permanent, 10000, worker, [FullName]},
    process:start(ChildSpec).

%% @doc gen_server entry
start_link() ->
    start_link(?MODULE, []).
start_link(FullName, Args) ->
    gen_server:start_link({local, FullName}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, local, Type, Limit, Key, Value, Time, Rank, Data]) ->
    %% make new sorter
    Sorter = sorter:new(Name, local, Type, Limit, Key, Value, Time, Rank, Data),
    {ok, #state{sorter = Sorter}};
init(_) ->
    {ok, #state{}}.

handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({'new', Name, local, Type, Limit, Key, Value, Time, Rank, Data}, State) ->
    %% make new sorter
    Sorter = ?STACK_TRACE(sorter:new(Name, local, Type, Limit, Key, Value, Time, Rank, Data)),
    {noreply, State#state{sorter = Sorter}};
handle_info({'update', Data}, State = #state{sorter = Sorter}) ->
    %% update online player info cache
    ?STACK_TRACE(sorter:update(Data, Sorter)),
    {noreply, State};
handle_info('stop', State) ->
    {stop, normel, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
