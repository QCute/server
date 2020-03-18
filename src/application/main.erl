%%%------------------------------------------------------------------
%%% @doc
%%% module main application
%%% @end
%%%------------------------------------------------------------------
-module(main).
-behaviour(application).
%% gracefully
-export([stop_safe/0, stop_safe/1]).
%% API
-export([start/0, stop/0]).
%% application callbacks
-export([start/2, stop/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start main application
-spec start() -> ok | {error, term()}.
start() ->
    %% process pool
    application:start(volley),
    %% main application
    application:start(?MODULE).

%% @doc stop main application
-spec stop() -> ok.
stop() ->
    %% main application
    application:stop(?MODULE),
    %% process pool
    application:stop(volley),
    %% exit
    init:stop().

%% @doc stop application safely
-spec stop_safe() -> ok | {error, term()}.
stop_safe() ->
    %% close tcp entry
    catch user_manager:set_server_state(refuse),
    %% stop role server, wait for all server stop
    catch user_manager:stop_all(),
    %% normal stop all server
    stop().

%% @doc remote stop application safely
-spec stop_safe(Nodes :: [atom()]) -> true.
stop_safe(NodeList) ->
    Self = self(),
    List = [spawn(fun() -> erlang:send(Self, {Node, rpc:call(Node, main, stop_safe, [])}) end) || Node <- NodeList],
    [receive {Node, Result} -> io:format("node:~w result:~w~n", [Node, Result]) end || _ <- List].

%%%==================================================================
%%% application callbacks
%%%==================================================================
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
%%%==================================================================
%%% Internal functions
%%%==================================================================
%% (System Architecture Support Libraries)
