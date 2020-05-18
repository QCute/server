%%%-------------------------------------------------------------------
%%% @doc
%%% module main application
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-behaviour(application).
%% API
-export([start/0, stop/0, stop_remote/1]).
%% application callbacks
-export([start/2, prep_stop/1, stop/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start main application
-spec start() -> ok | {error, term()}.
start() ->
    %% main application
    application:start(?MODULE).

%% @doc stop main application
-spec stop() -> ok.
stop() ->
    %% stop role server
    catch user_manager:stop_all(),
    %% stop data server
    application:stop(?MODULE),
    %% normal stop
    init:stop().

%% @doc stop remote application
-spec stop_remote(NodeList :: [node()]) -> ok.
stop_remote(NodeList) ->
    Self = self(),
    List = [spawn(fun() -> erlang:send(Self, {Node, rpc:call(Node, ?MODULE, stop, [])}) end) || Node <- NodeList],
    lists:foreach(fun(_) -> receive {Node, Result} -> io:format("node:~w result:~w~n", [Node, Result]) end end, List).

%%%===================================================================
%%% application callbacks
%%%===================================================================
%% @doc start application
-spec start(StartType :: term(), StartArgs :: list()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_, _) ->
    %% inets
    inets:start(),
    %% ssl(crypto)
    ssl:start(),
    %% get node type
    {ok, NodeType} = application:get_env(node_type),
    %% start service
    service:start(NodeType).

%% @doc prepare stop application
-spec prep_stop(State :: term()) -> ok.
prep_stop(State) ->
    %% get node type
    {ok, NodeType} = application:get_env(node_type),
    %% stop service
    service:stop(NodeType),
    %% return state to master
    State.

%% @doc stop application
-spec stop(State :: term()) -> ok.
stop(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% (System Architecture Support Libraries)
