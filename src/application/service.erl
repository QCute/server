%%%-------------------------------------------------------------------
%%% @doc
%%% service start/stop control
%%% @end
%%%-------------------------------------------------------------------
-module(service).
%% API
-export([start/1, stop/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start local node services
-spec start(Node :: local | center | world) -> {ok, pid()}.
start(Node = local) ->
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% database connector
    {ok, _} = db:start(),
    %% database initialize
    ok = db:initialize(),
    %% path find
    {ok, _} = path_finder:start(),
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% console
    {ok, _} = console:start(),
    %% node
    {ok, _} = node:start(Node),
    %% increment
    {ok, _} = increment_server:start(),
    %% log
    {ok, _} = log_server:start(),
    %% user manager
    {ok, _} = user_manager:start(),
    %% guild
    {ok, _} = guild_server:start(),
    %% rank
    rank_server:start(Node),
    %% city map
    map_server:start_city(),
    %% activity
    {ok, _} = activity_server:start(Node),
    %% boss
    {ok, _} = boss_server:start(),
    %% key
    {ok, _} = key_server:start(),
    %% auction
    {ok, _} = auction_server:start(),
    %% lucky money
    {ok, _} = lucky_money_server:start(),

    %% start normal service before @here ↑
    %% start network io service after normal service started ↓
    %% network io supervisor
    {ok, _} = net_supervisor:start_link(),
    %% tcp or ssl
    {ok, _} = listener:start(),
    %% mirror service supervisor
    {ok, Pid};

%% @doc start center node services
start(Node = center) ->
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% path find
    {ok, _} = path_finder:start(),
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% console
    {ok, _} = console:start(),
    %% node
    {ok, _} = node:start(Node),
    %% rank
    ok = rank_server:start(Node),

    %% start normal service before @here ↑
    %% mirror service supervisor
    {ok, Pid};

%% @doc start world node services
start(Node = world) ->
    %% volley process pool
    {ok, _} = volley:start_link(),
    %% path find
    {ok, _} = path_finder:start(),
    %% server supervisor
    {ok, Pid} = service_supervisor:start_link(),
    %% console
    {ok, _} = console:start(),
    %% node
    {ok, _} = node:start(Node),
    %% rank
    ok = rank_server:start(Node),

    %% start normal service before @here ↑
    %% mirror service supervisor
    {ok, Pid}.

%% @doc stop local node services
-spec stop(Node :: local | center | world) -> ok.
stop(local) ->
    %% stop network first
    gen_server:stop(net_supervisor),
    %% stop normal service
    gen_server:stop(service_supervisor),
    %% stop database pool
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
