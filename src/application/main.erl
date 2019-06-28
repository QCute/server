%%%-------------------------------------------------------------------
%%% @doc
%%% module main application
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-behaviour(application).
%% remote
-export([stop_safe/1]).
%% gracefully
-export([stop_safe/0]).
%% API
-export([start/0, stop/0]).
%% application callbacks
-export([start/2, stop/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start main application
-spec start() -> 'ok' | {'error', term()}.
start() ->
    %% process pool
    application:start(volley),
    %% main application
    application:start(?MODULE).

%% @doc stop main application
-spec stop() -> 'ok' | {'error', term()}.
stop() ->
    %% main application
    application:stop(?MODULE),
    %% process pool
    application:stop(volley),
    %% exit
    init:stop().

-spec stop_safe() -> 'ok' | {'error', term()}.
stop_safe() ->
    %% close tcp entry
    catch player_manager:change_server_state(false),
    %% stop player server
    catch player_manager:stop_all(),
    %% wait for save all data
    timer:sleep(1000),
    %% normal stop all server
    stop().

-spec stop_safe(Nodes :: [atom()] | [list()]) -> true.
stop_safe(NodeList) ->
    [net_adm:ping(Node) == pong andalso rpc:cast(type:to_atom(Node), ?MODULE, stop_safe, []) || Node <- NodeList].

%%%===================================================================
%%% application callbacks
%%%===================================================================
%% @doc start application
-spec start(term(), list()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_, _) ->
    %% inets
    inets:start(),
    %% ssl(crypto)
    ssl:start(),
    %% get node type
    {ok, NodeType} = application:get_env(node_type),
    %% start this node services and return child pid
    service:start(NodeType).

%% @doc stop application
-spec stop(term()) -> ok.
stop(_) ->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% (System Architecture Support Libraries)
