%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(service).
%% API
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start local node services
-spec start(Type :: local | center | world) -> {ok, pid()}.
start(Type = local) ->
    %% database connector
    {ok, _} = sql:start(),
    %% database initialize
    %% ok = sql:initialize(),
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% node server
    {ok, _} = node:start(Type),
    %% increment server
    {ok, _} = increment_server:start(),
    %% log
    {ok, _} = log_server:start(),
    %% guild
    {ok, _} = guild_server:start(),
    %% user manager
    {ok, _} = user_manager:start(),
    %% key
    {ok, _} = key_server:start(),
    %% rank
    ok = rank_server:start(Type),
    %% path find
    {ok, _} = path_finder:start(),
    %% map
    map_server:start_city(),
    %% activity
    {ok, _} = activity_server:start(Type),
    %% boss
    {ok, _} = boss_server:start(),
    %% auction
    {ok, _} = auction_server:start(),
    %% lucky money
    {ok, _} = lucky_money_server:start(),
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
    %% node server
    {ok, _} = node:start(Type),
    %% log
    {ok, _} = log_server:start(),
    %% rank
    ok = rank_server:start(Type),
    %% application child server supervisor
    {ok, Pid};

%% @doc start world node services
start(Type = world) ->
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% node server
    {ok, _} = node:start(Type),
    %% log
    {ok, _} = log_server:start(),
    %% rank
    ok = rank_server:start(Type),
    %% application child server supervisor
    {ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
