%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(service).
%% API
-export([start/1, stop/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start local node services
-spec start(Type :: local | center | world) -> {ok, pid()}.
start(Type = local) ->
    %% @doc start permanent server pool start of here
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% database connector
    {ok, _} = sql:start(),
    %% database initialize
    ok = sql:initialize(),
    %% path find
    {ok, _} = path_finder:start(),
    %% @doc start permanent server start of here
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% node server
    {ok, _} = node:start(Type),
    %% increment server
    {ok, _} = increment_server:start(),
    %% log
    {ok, _} = log_server:start(),
    %% user manager
    {ok, _} = user_manager:start(),
    %% guild
    {ok, _} = guild_server:start(),
    %% rank
    rank_server:start(Type),
    %% city map
    map_server:start_city(),
    %% activity
    {ok, _} = activity_server:start(Type),
    %% boss
    {ok, _} = boss_server:start(),
    %% key
    {ok, _} = key_server:start(),
    %% auction
    {ok, _} = auction_server:start(),
    %% lucky money
    {ok, _} = lucky_money_server:start(),
    %% @doc start net server start of here
    %% network io
    {ok, _} = net_supervisor:start_link(),
    %% tcp or ssl
    {ok, _} = listener:start(),
    %% mirror service supervisor
    {ok, Pid};

%% @doc start center node services
start(Type = center) ->
    %% @doc start permanent server pool start of here
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% path find
    {ok, _} = path_finder:start(),
    %% @doc start permanent server start of here
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% node server
    {ok, _} = node:start(Type),
    %% rank
    ok = rank_server:start(Type),
    %% mirror service supervisor
    {ok, Pid};

%% @doc start world node services
start(Type = world) ->
    %% @doc start permanent server pool start of here
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% path find
    {ok, _} = path_finder:start(),
    %% @doc start permanent server start of here
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% node server
    {ok, _} = node:start(Type),
    %% rank
    ok = rank_server:start(Type),
    %% mirror service supervisor
    {ok, Pid}.

%% @doc stop local node services
-spec stop(Type :: local | center | world) -> ok.
stop(local) ->
    gen_server:stop(net_supervisor),
    gen_server:stop(service_supervisor),
    gen_server:stop(volley);

%% @doc stop center node services
stop(center) ->
    gen_server:stop(service_supervisor),
    gen_server:stop(volley);

%% @doc stop world node services
stop(world) ->
    gen_server:stop(service_supervisor),
    gen_server:stop(volley).

%%%===================================================================
%%% Internal functions
%%%===================================================================
