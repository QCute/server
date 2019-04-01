%%%-------------------------------------------------------------------
%%% @doc
%%% module database worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(data_pool).
-export([start/0]).
%% includes
-include("common.hrl").
-define(POOL_DB_GAME_THREAD_NUMBER,   16).
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc start db pool
start() ->
    start_pool(?POOL, ?POOL_DB_GAME_THREAD_NUMBER).

%% ====================================================================
%% Internal functions
%% ====================================================================
start_pool(Pool, Size) ->
    %% database name as pool id
    PoolArg = [{name, {local, Pool}}, {worker_module, mysql_driver}, {size, Size}, {max_overflow, 0}, {strategy, lifo}],
    %% read config from application env
    {ok, List} = application:get_env(Pool),
    %% start pool boy
    poolboy:start_link(PoolArg, List).
