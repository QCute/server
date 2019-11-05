%% volley_pool_sup manage ets and child process
-module(volley_pool_sup).
-behaviour(supervisor).
-export([start_link/2, name/1]).
%% supervisor callback
-export([init/1, start_worker/3]).

-spec start_link(atom(), term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, name(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

-spec name(atom()) -> atom().
name(PoolName) ->
    list_to_atom(lists:concat([?MODULE, "_", PoolName])).

%%%===================================================================
%%% supervisor callback
%%%===================================================================
-spec init(Args :: term()) -> {ok, term()}.
init([PoolName, PoolArgs]) ->
    PoolSize        = proplists:get_value(size, PoolArgs, 1),
    RestartPeriod   = proplists:get_value(period, PoolArgs, 1),
    Intensity       = proplists:get_value(intensity, PoolArgs, 1000),
    RestartStrategy = proplists:get_value(restart, PoolArgs, permanent),
    Worker          = proplists:get_value(worker, PoolArgs),
    %% pool table
    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{seq, 0}, {size, PoolSize}]),
    %% construct child 
    Child = lists:map(fun(Id) -> {Id, {?MODULE, start_worker, [Id, PoolTable, Worker]}, RestartStrategy, infinity, worker, [?MODULE]} end, lists:seq(1, PoolSize)),
    {ok, {{one_for_one, Intensity, RestartPeriod}, Child}}.

-spec start_worker(Id :: non_neg_integer(), PoolTable :: atom(), MFA :: {atom(), atom(), term()}) -> {ok, pid()}.
start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    erlang:link(Pid),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.
