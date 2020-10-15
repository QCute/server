%%%-------------------------------------------------------------------
%%% @doc
%%% main application
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-behaviour(application).
%% API
-export([debug/0]).
-export([start/0, stop/0, stop_remote/1, reload_env/0]).
%% application callbacks
-export([start/2, prep_stop/1, stop/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% Intellij Idea Debug Guild
%% Run -> Edit Configurations -> Add -> Erlang Application
%% Parameter set like this:
%% Name                   : local
%% Module and function    : main debug
%% Function arguments     :
%% Working Directory      : (project root path, to system absolute path)
%% Flags for 'erl'        : +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds 2 +P 1048576 +t 1048576 +zdbbl 1024 -setcookie erlang -name local@127.0.0.1 -config config/local -boot start_sasl
%% Before launch          : (remove build option(default open))
%% Active tool window     : true

%% @doc debug
-spec debug() -> no_return().
debug() ->
    application:start(?MODULE),
    timer:sleep(infinity).

%% @doc start main application
-spec start() -> ok | {error, term()}.
start() ->
    %% main application
    application:start(?MODULE).

%% @doc stop main application
-spec stop() -> ok.
stop() ->
    %% stop role server
    catch user_manager:update_notify(),
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

%% @doc reload application env
-spec reload_env() -> ok.
reload_env() ->
    {ok, List} = init:get_argument(config),
    [begin {ok, [Config]} = file:consult(lists:concat([Name, ".config"])), [application:unset_env(?MODULE, Key) || {Key, _} <- application:get_all_env(?MODULE)], [application:set_env(?MODULE, Key, Value) || {Key, Value} <- proplists:get_value(?MODULE, Config, [])] end || [Name] <- List],
    ok.

%%%===================================================================
%%% application callbacks
%%%===================================================================
%% @doc start application
-spec start(StartType :: term(), StartArgs :: list()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_, _) ->
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
