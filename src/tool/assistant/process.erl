%%%-------------------------------------------------------------------
%%% @doc
%%% module process
%%% @end
%%%-------------------------------------------------------------------
-module(process).
%% API
-export([start/1, start/2, start/3]).
-export([pid/1, where/1, alive/1]).
-export([call/2]).
%% Includes
-include("common.hrl").
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
        {badrpc, _Reason}  ->
            false;
        Result ->
            Result
    end.

%% @doc call with timeout
-spec call(Pid :: pid() | atom(), Request :: term()) -> Reply :: term().
call(Pid, Request) ->
    case catch gen_server:call(Pid, Request, ?CALL_TIMEOUT) of
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        Reply ->
            Reply
    end.
