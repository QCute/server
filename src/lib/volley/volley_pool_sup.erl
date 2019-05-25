-module(volley_pool_sup).
-behaviour(supervisor).
-export([start_link/2, name/1]).
-export([init/1, start_worker/3]).

start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, name(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

name(PoolName) ->
    list_to_atom(lists:concat([?MODULE, "_", PoolName, "_sup"])).

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
    Child = lists:map(fun(Id) -> children_specs(Id, RestartStrategy, [Id, PoolTable, Worker]) end, lists:seq(1, PoolSize)),
    {ok, {{one_for_one, Intensity, RestartPeriod}, Child}}.

start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.

children_specs(Name, SupRestartStrategy, Args) ->
    {Name, {?MODULE, start_worker, Args}, SupRestartStrategy, 2000, worker, [?MODULE]}.