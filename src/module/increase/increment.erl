%%%-------------------------------------------------------------------
%%% @doc
%%% module increment
%%% @end
%%%-------------------------------------------------------------------
-module(increment).
-behaviour(gen_server).
%% API
-export([next/1, new/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc next id
-spec next(Name :: atom()) -> non_neg_integer().
next(Name) ->
    %% Threshold is max integer
    ets:update_counter(Name, sequence, {2, 1, 4294967296, 1}).

%% @doc add new increase table
-spec new(Name :: atom()) -> ok.
new(Name) ->
    gen_server:cast(?MODULE, {new, Name}).

%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    {ok, []}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({new, Name}, State) ->
    Tab = ets:new(Name, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(Tab, [{sequence, 0}]),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
