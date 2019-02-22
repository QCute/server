%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(services).
%% API
-export([start/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start local node services
-spec start(Type :: local | center | big_world) -> {'ok', SuperVisorPid :: pid()}.
start(local) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = rand:start(),
    %% database connect pool
    {ok, _} = data_pool:start(),
    %% node server
    {ok, _} = node_server:start(local),
    %% guild
    {ok, _} = guild_server:start(),
    %% player manager
    {ok, _} = player_manager:start(),
    %% key
    {ok, _} = key_server:start(),
    %% rank
    ok = rank_server:start_all(local),
    %% common service should start before the io service
    %% netword io part
    %% server io listener/acceptor/receiver
    {ok, _} = main_supervisor:start_link(),
    %% general tcp
    {ok, _} = listener:start_gen_tcp(),
    %% tcp with ssl
    {ok, _} = listener:start_ssl(),
    %% application child server supervisor
    {ok, Pid};

%% @doc start center node services
start(center) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = rand:start(),
    %% node server
    {ok, _} = node_server:start(center),
    %% rank
    ok = rank_server:start_all(center),
    %% application child server supervisor
    {ok, Pid};

%% @doc start big world node services
start(big_world) ->
    %% server supervisor
    {ok, Pid} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = rand:start(),
    %% node server
    {ok, _} = node_server:start(big_world),
    %% rank
    ok = rank_server:start_all(big_world),
    %% application child server supervisor
    {ok, Pid}.
%%%===================================================================
%%% Internal functions
%%%===================================================================