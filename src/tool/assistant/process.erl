%%%-------------------------------------------------------------------
%%% @doc
%%% process tool
%%% @end
%%%-------------------------------------------------------------------
-module(process).
%% API
-export([start/1, start/2, start/3]).
-export([is_alive/1]).
-export([send_after/2, start_timer/2, cancel_timer/1]).
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

%% @doc send after seconds
-spec send_after(Time :: non_neg_integer(), Message :: term()) -> reference().
send_after(0, _Message) ->
    undefined;
send_after(Time, Message) ->
    erlang:send_after(?SECOND_MILLISECONDS(Time), self(), Message).

%% @doc send after seconds
-spec start_timer(Time :: non_neg_integer(), Message :: term()) -> reference().
start_timer(0, _Message) ->
    undefined;
start_timer(Time, Message) ->
    erlang:start_timer(?SECOND_MILLISECONDS(Time), self(), Message).

%% @doc cancel timer catch error
-spec cancel_timer(Timer :: reference()) -> non_neg_integer() | false.
cancel_timer(undefined) ->
    0;
cancel_timer(Timer) ->
    catch erlang:cancel_timer(Timer).
