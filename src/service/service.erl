%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(service).
%% API
-export([start/1]).
%% Includes
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start local node services
-spec start(Type :: local | center | big_world) -> {'ok', SupervisorPid :: pid()}.
start(Type = local) ->
    %% database connect pool (manage by volley application group)
    {ok, _} = mysql_connector:start_pool(),
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% increase
    {ok, _} = increment:start(),
    %% node server
    {ok, _} = node_server:start(Type),
    %% guild
    {ok, _} = guild_server:start(),
    %% role manager
    {ok, _} = user_manager:start(),
    %% key
    {ok, _} = key_server:start(),
    %% rank
    ok = rank_server:start_all(Type),
    %% path find
    {ok, _} = path_finder:start(),
    %% map
    {ok, _} = map_server:start(),
    %% auction
    {ok, _} = auction_server:start(),
    %% common service should start before the io service
    %% network io part
    %% server io listener/acceptor/receiver
    {ok, _} = net_supervisor:start_link(),
    %% general tcp
    {ok, _} = listener:start_gen_tcp(),
    %% tcp with ssl
    {ok, _} = listener:start_ssl(),
    %% application child server supervisor
    {ok, Pid};

%% @doc start center node services
start(Type = center) ->
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% increase
    {ok, _} = increment:start(),
    %% node server
    {ok, _} = node_server:start(Type),
    %% rank
    ok = rank_server:start_all(Type),
    %% application child server supervisor
    {ok, Pid};

%% @doc start big world node services
start(Type = big_world) ->
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = randomness:start(),
    %% increase
    {ok, _} = increment:start(),
    %% node server
    {ok, _} = node_server:start(Type),
    %% rank
    ok = rank_server:start_all(Type),
    %% application child server supervisor
    {ok, Pid}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
