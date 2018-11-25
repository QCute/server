-module(behavior_gen_statem).
-behaviour(gen_statem).
-export([start_link/0, set_socket/2,callback_mode/0]).
-export([init/1, handle_event/4,terminate/3, code_change/4]).

-define(PACKET,0).
-define(PACKET_TYPE,list).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, []}.

callback_mode() ->
    handle_event_function.


handle_event(timeout, {conn_timeout}, wait_for_data, State) ->
    {stop, normal, State};

handle_event(info, {tcp, _Socket, _Bin}, wait_for_data, State) ->
    {stop, normal, State};

handle_event(info, {tcp_error, _, _}, _, State) ->
    {stop, normal, State};

handle_event(info, {tcp_closed, _, _}, _, State) ->
    {stop, normal, State};

handle_event(info, _Event, _, State) ->  
    {stop, normal, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, OldState, StateData, _Extra) ->
    {ok, OldState, StateData}.