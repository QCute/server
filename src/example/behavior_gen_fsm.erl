-module(behavior_gen_fsm).
-compile(nowarn_deprecated_function).
-behaviour(gen_fsm).
-export([start_link/0, set_socket/2,callback_mode/0]).
-export([init/1, handle_event/3,handle_sync_event/4,handle_info/3,terminate/3, code_change/4]).

-define(PACKET,0).
-define(PACKET_TYPE,list).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:sync_send_event(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, []}.

callback_mode() ->
    handle_event_function.


handle_event(timeout, {conn_timeout}, State) ->
    {stop, normal, State};

handle_event(timeout, {tcp, _Socket, _Bin}, State) ->
    {stop, normal, State};

handle_event(timeout, {tcp_error, _, _}, State) ->
    {stop, normal, State};

handle_event(timeout, {tcp_closed, _, _}, State) ->
    {stop, normal, State};

handle_event(timeout, _Event, State) ->
    {stop, normal, State}.

handle_sync_event(_Any, _From, StateName, State) ->
    {reply, {error, unhandled}, StateName, State}.

handle_info(_Any, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, OldState, StateData, _Extra) ->
    {ok, OldState, StateData}.