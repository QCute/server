%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(services).
%% API
-export([start/1]).
%% Includes
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start local node services
-spec start(Type :: local | center | big_world) -> {'ok', SuperVisorPid :: pid()}.
start(Type = local) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% database connect pool
    {ok, _} = mysql_driver:start_pool(?POOL),
    %% node server
    {ok, _} = node_server:start(Type),
    %% guild
    {ok, _} = guild_server:start(),
    %% player manager
    {ok, _} = player_manager:start(),
    %% key
    {ok, _} = key_server:start(),
    %% rank
    ok = rank_server:start_all(Type),
    %% common service should start before the io service
    %% network io part
    %% server io listener/acceptor/receiver
    {ok, _} = main_supervisor:start_link(),
    %% general tcp
    {ok, _} = listener:start_gen_tcp(),
    %% tcp with ssl
    {ok, _} = listener:start_ssl(),
    %% application child server supervisor
    {ok, Pid};

%% @doc start center node services
start(Type = center) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% node server
    {ok, _} = node_server:start(Type),
    %% rank
    ok = rank_server:start_all(Type),
    %% application child server supervisor
    {ok, Pid};

%% @doc start big world node services
start(Type = big_world) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% node server
    {ok, _} = node_server:start(Type),
    %% rank
    ok = rank_server:start_all(Type),
    %% application child server supervisor
    {ok, Pid}.
%%%===================================================================
%%% Internal functions
%%%===================================================================