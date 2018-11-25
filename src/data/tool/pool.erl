%%%-------------------------------------------------------------------
%%% @doc
%%% module database worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(pool).
-include("common.hrl").
-export([start/0]).
-define(DB_GAME_THREAD_NUMBER,   16).
-define(DB_ADMIN_THREAD_NUMBER,  4).

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
    start_pool(?DB_GAME, ?DB_GAME_THREAD_NUMBER, 0),
    %start_pool(?DB_ADMIN, ?DB_ADMIN_THREAD_NUMBER, 0),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_pool(Pool, Size, MaxOverflow) ->
    PoolArg = [{worker_module, mysql_conn}, {size, Size}, {max_overflow, MaxOverflow}, {strategy, lifo}],
    WorkerArgs = read_from_env(database),
    poolboy:start_link(Pool, PoolArg, WorkerArgs),
    ok.

%% database configure
read_from_env(Configure) ->
    case application:get_env(Configure) of
        {ok, Cfg} ->
            [Host, Port, User, Password, DB, Encode] = fetch_param(Cfg),
            %% database name as pool id
            [Host, Port, User, Password, DB, fun(_, _, _, _) -> ok end, Encode, Configure];
        _ ->
            throw(undefined)
    end.

fetch_param(Cfg) ->
    {_, Host} = lists:keyfind(host, 1, Cfg),
    {_, Port} = lists:keyfind(port, 1, Cfg),
    {_, User} = lists:keyfind(user, 1, Cfg),
    {_, Password} = lists:keyfind(password, 1, Cfg),
    {_, DB} = lists:keyfind(database, 1, Cfg),
    {_, Encode} = lists:keyfind(encode, 1, Cfg),
    [Host, Port, User, Password, DB, Encode].
