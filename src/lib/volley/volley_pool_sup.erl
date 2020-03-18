%%%------------------------------------------------------------------
%%% @doc
%%% volley_pool_sup
%%% process pool and ets manager
%%% @end
%%%------------------------------------------------------------------
-module(volley_pool_sup).
-behaviour(supervisor).
-export([start_link/2, name/1, make_worker/3, make_worker/5]).
%% supervisor callback
-export([init/1, start_worker/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start link
-spec start_link(atom(), term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, name(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

%% @doc supervisor name
-spec name(atom()) -> atom().
name(PoolName) ->
    list_to_atom(lists:concat([?MODULE, "_", PoolName])).

%% @doc make worker
-spec make_worker(Id :: non_neg_integer(), PoolTable :: atom() | ets:tab(), PoolArgs :: term()) -> supervisor:child_spec().
make_worker(Id, PoolTable, PoolArgs) ->
    %% pool
    Worker          = proplists:get_value(worker, PoolArgs),
    %% supervisor
    RestartStrategy = proplists:get_value(restart, PoolArgs, permanent),
    Shutdown        = proplists:get_value(shutdown, PoolArgs, infinity),
    %% make worker (supervisor spec)
    make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown).

%% @doc make worker
-spec make_worker(Id :: non_neg_integer(), PoolTable :: atom() | ets:tab(), Worker :: {module(), atom(), term()}, RestartStrategy :: atom(), Shutdown :: atom()) -> supervisor:child_spec().
make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown) ->
    {Id, {?MODULE, start_worker, [Id, PoolTable, Worker]}, RestartStrategy, Shutdown, worker, [?MODULE]}.

%%%==================================================================
%%% supervisor callback
%%%==================================================================
-spec init(Args :: term()) -> {ok, term()}.
init([PoolName, PoolArgs]) ->
    %% pool
    PoolSize        = proplists:get_value(size, PoolArgs, 1),
    Worker          = proplists:get_value(worker, PoolArgs),
    %% supervisor
    RestartPeriod   = proplists:get_value(period, PoolArgs, 1),
    Intensity       = proplists:get_value(intensity, PoolArgs, 1000),
    RestartStrategy = proplists:get_value(restart, PoolArgs, permanent),
    Shutdown        = proplists:get_value(shutdown, PoolArgs, infinity),
    %% pool table
    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{seq, 0}, {size, PoolSize}]),
    %% construct child 
    Child = lists:map(fun(Id) -> make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown) end, lists:seq(1, PoolSize)),
    {ok, {{one_for_one, Intensity, RestartPeriod}, Child}}.

-spec start_worker(Id :: non_neg_integer(), PoolTable :: atom(), Worker :: {module(), atom(), term()}) -> {ok, pid()}.
start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    erlang:link(Pid),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.
