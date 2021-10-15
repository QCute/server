%%%-------------------------------------------------------------------
%%% @doc
%%% process tool
%%% @end
%%%-------------------------------------------------------------------
-module(process).
%% API
-export([start/1, start/2, start/3]).
-export([is_alive/1]).
%% Includes
-include("time.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(Name :: atom()) -> {ok, pid()} | {error, term()}.
start(Name) ->
    start(Name, []).
-spec start(Name :: atom(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start(Name, Args) ->
    start(Name, Name, Args).
-spec start(Name :: atom(), Module :: module(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start(Name, Module, Args) ->
    %% kill(force termination) worker server after 60 seconds
    ChildSpec = {Name, {Module, start_link, Args}, permanent, 60000, worker, [Name]},
    service_supervisor:start_child(ChildSpec).

%% @doc process is alive
-spec is_alive(Pid :: pid()) -> boolean().
is_alive(undefined) ->
    false;
is_alive(Pid) when is_pid(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid], ?CALL_TIMEOUT) of
        true ->
            true;
        _ ->
            false
    end.
