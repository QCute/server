%% date 2019/05/25
%% an erlang process pool inspired from pool-boy, erl-pool, cue-sport, revolver
%% simple and fast
%% easy to integrate to your project
%% enjoy it
-module(volley).
-export([get/1, map/2, start_pool/2, stop_pool/1]).
-export_type([pool_option/0]).
-type pool_option():: {worker, {module(), atom(), [term()]}} | {size, non_neg_integer()} | {period, non_neg_integer()} | {intensity, non_neg_integer()} | {restart, supervisor:restart()}.

-spec get(atom()) -> {ok, pid()} | {error, any()}.
get(PoolName) ->
    try
        {ok, PoolSize} = pool_size(PoolName),
        N = ets:update_counter(PoolName, seq, {2, 1, PoolSize, 1}),
        [{N, Worker}] = ets:lookup(PoolName, N),
        {ok, Worker}
    catch _:Error ->
        {error, Error}
    end.

-spec map(atom(), fun((pid()) -> any())) -> [term()] | {error, any()}.
map(PoolName, F) ->
    try
        %% match spec generate by ets:fun2ms(fun({_, Pid}) when is_pid(Pid) -> Pid end).
        lists:map(F, ets:select(PoolName, [{{'_', '$1'}, [{is_pid, '$1'}], ['$1']}]))
    catch _:Error ->
        {error, Error}
    end.

-spec start_pool(atom(), [pool_option()]) -> {ok, pid()} | {ok, pid(), term()} | {error, any()}.
start_pool(PoolName, PoolArgs) ->
    volley_sup:add_pool(PoolName, PoolArgs).

-spec stop_pool(atom()) -> ok | {error, any()}.
stop_pool(PoolName) ->
    volley_sup:remove_pool(PoolName).

-spec pool_size(atom()) -> {ok, non_neg_integer()} | {error, any()}.
pool_size(PoolName) ->
    try
        {ok, ets:lookup_element(PoolName, size, 2)}
    catch _:Error  ->
        {error, Error}
    end.
