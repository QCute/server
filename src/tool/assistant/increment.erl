%%%------------------------------------------------------------------
%%% @doc
%%% module increment
%%% @end
%%%------------------------------------------------------------------
-module(increment).
-behaviour(gen_server).
%% API
-export([next/0, next/1, new/1, new/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc next id
-spec next() -> non_neg_integer().
next() ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, sequence, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc next id
-spec next(Name :: atom()) -> non_neg_integer().
next(Name) ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(Name, sequence, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc add new increase table
-spec new(Name :: atom()) -> ok.
new(Name) when is_atom(Name) ->
    gen_server:cast(?MODULE, {new, Name}).

%% @doc add new increase table
-spec new(Name :: atom(), Begin :: non_neg_integer()) -> ok.
new(Name, Begin) when is_atom(Name) andalso is_integer(Begin) ->
    gen_server:cast(?MODULE, {new, Name, Begin}).

%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([]) ->
    %% default increment id
    ets:new(?MODULE, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(?MODULE, [{sequence, 0}]),
    %% map
    ets:new(map, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(map, [{sequence, 0}]),
    %% monster
    ets:new(monster, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(monster, [{sequence, 100000}]),
    {ok, []}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({new, Name}, State) ->
    catch ets:new(Name, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    catch ets:insert(Name, [{sequence, 0}]),
    {noreply, State};
handle_cast({new, Name, Begin}, State) ->
    catch ets:new(Name, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    catch ets:insert(Name, [{sequence, Begin}]),
    {noreply, State};
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
