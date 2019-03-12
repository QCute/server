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
    PoolArg = [{name, {local, Pool}}, {worker_module, mysql_conn}, {size, Size}, {max_overflow, 0}, {strategy, lifo}],
    %% read config from application env
    {ok, List} = application:get_env(Pool),
    {_, Host} = lists:keyfind(host, 1, List),
    {_, Port} = lists:keyfind(port, 1, List),
    {_, User} = lists:keyfind(user, 1, List),
    {_, Password} = lists:keyfind(password, 1, List),
    {_, DataBase} = lists:keyfind(database, 1, List),
    {_, Encode} = lists:keyfind(encode, 1, List),
    %% config mysql connection
    WorkerArgs = [Host, Port, User, Password, DataBase, fun(_, _, _, _) -> ok end, Encode, Pool],
    %% start poolboy
    poolboy:start_link(PoolArg, WorkerArgs).
