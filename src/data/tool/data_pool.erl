%%%-------------------------------------------------------------------
%%% @doc
%%% module database worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(data_pool).
-include("common.hrl").
-export([start/0]).
-define(POOL_DB_GAME_THREAD_NUMBER,   16).

-ifdef(DEBUG).
-define(DEBUG_PRINT, true).
-else.
-define(DEBUG_PRINT, false).
-endif.
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc start db pool
start() ->
    start_pool(?POOL, ?POOL_DB_GAME_THREAD_NUMBER, 0),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_pool(Pool, Size, MaxOverflow) ->
    PoolArg = [{name, {local, Pool}}, {worker_module, mysql_conn}, {size, Size}, {max_overflow, MaxOverflow}, {strategy, lifo}],
    WorkerArgs = read_from_env(Pool),
    poolboy:start_link(PoolArg, WorkerArgs),
    ok.

%% database configure
read_from_env(Configure) ->
    {ok, Cfg} = application:get_env(Configure),
    [Host, Port, User, Password, DB, Encode] = fetch_param(Cfg),
    %% database name as pool id
    [Host, Port, User, Password, DB, fun(_, _, _, _) -> ok end, Encode, Configure].


fetch_param(Cfg) ->
    {_, Host} = lists:keyfind(host, 1, Cfg),
    {_, Port} = lists:keyfind(port, 1, Cfg),
    {_, User} = lists:keyfind(user, 1, Cfg),
    {_, Password} = lists:keyfind(password, 1, Cfg),
    {_, DB} = lists:keyfind(database, 1, Cfg),
    {_, Encode} = lists:keyfind(encode, 1, Cfg),
    [Host, Port, User, Password, DB, Encode].
