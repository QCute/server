%%%-------------------------------------------------------------------
%%% @doc
%%% module main, to start application
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-behaviour(application).
%% export API function
-export([start/0, stop/0]).
%% application callbacks
-export([start/2, stop/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start main application
-spec start() -> 'ok' | {'error', term()}.
start() ->
    application:start(?MODULE).

%% @doc stop main application
-spec stop() -> 'ok' | {'error', term()}.
stop() ->
    application:stop(?MODULE).
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
    %% start all services
    services:start_services(),
    %% start io services
    services:start_io().

%% @doc stop application
-spec stop(term()) -> ok.
stop(_) ->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% (System Architecture Support Libraries)