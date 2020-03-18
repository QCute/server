%%%------------------------------------------------------------------
%%% @doc
%%% volley
%%% * a lightweight erlang process pool inspired from pool-boy, erl-pool, cue-sport, revolver
%%% * simple and fast
%%% * easy to integrate in your project
%%% @end
%%%------------------------------------------------------------------
-module(volley).
-export([get/1, map/2]).
-export([start_pool/2, stop_pool/1]).
-export([pool_size/1, change_size/2]).
-export_type([pool_option/0]).
-type pool_option():: {worker, {module(), atom(), [term()]}} | {size, non_neg_integer()} | {period, non_neg_integer()} | {intensity, non_neg_integer()} | {shutdown, supervisor:shutdown()} | {restart, supervisor:restart()}.
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc get a worker
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

%% @doc worker map
-spec map(atom(), fun((pid()) -> any())) -> [term()] | {error, any()}.
map(PoolName, F) ->
    try
        %% match spec generate by ets:fun2ms(fun({_, Pid}) when is_pid(Pid) -> Pid end).
        lists:map(F, ets:select(PoolName, [{{'_', '$1'}, [{is_pid, '$1'}], ['$1']}]))
    catch _:Error ->
        {error, Error}
    end.

%% @doc start a pool
-spec start_pool(atom(), [pool_option()]) -> {ok, pid()} | {ok, pid(), term()} | {error, any()}.
start_pool(PoolName, PoolArgs) ->
    volley_sup:add_pool(PoolName, PoolArgs).

%% @doc stop a pool
-spec stop_pool(atom()) -> ok | {error, any()}.
stop_pool(PoolName) ->
    volley_sup:remove_pool(PoolName).

%% @doc get pool size
-spec pool_size(atom()) -> {ok, non_neg_integer()} | {error, any()}.
pool_size(PoolName) ->
    try
        {ok, ets:lookup_element(PoolName, size, 2)}
    catch _:Error  ->
        {error, Error}
    end.

%% @doc change pool size
-spec change_size(atom(), Size :: non_neg_integer()) -> boolean() | term().
change_size(PoolName, Size) ->
    try
        volley_sup:change_size(PoolName, Size)
    catch _:Error  ->
        Error
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
