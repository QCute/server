%%%-------------------------------------------------------------------
%%% @doc
%%% process tool
%%% @end
%%%-------------------------------------------------------------------
-module(process).
%% API
-export([start/1, start/2, start/3]).
-export([pid/1, where/1, alive/1]).
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

%% @doc process pid
-spec pid(Name :: atom() | {local, atom()} | {global, atom()}) -> pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(Name) ->
    where(Name).

%% @doc where
-spec where(Name :: term()) -> pid() | undefined.
where({local, Name}) ->
    erlang:whereis(Name);
where({global, Name}) ->
    global:whereis_name(Name);
where(Name) ->
    erlang:whereis(Name).

%% @doc process is alive
-spec alive(Pid :: pid()) -> boolean().
alive(undefined) ->
    false;
alive(Pid) when is_pid(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid], ?CALL_TIMEOUT) of
        true ->
            true;
        _ ->
            false
    end.
