%%%------------------------------------------------------------------
%%% @doc
%%% module receiver
%%% @end
%%%------------------------------------------------------------------
-module(receiver).
-behaviour(gen_server).
%% API
-export([start/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% socket state and socket error define
-include("socket.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> {ok, pid()} | {error, term()}.
start(SocketType, Socket) ->
    %% do not mirror by the net supervisor
    gen_server:start(?MODULE, [SocketType, Socket], []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([SocketType, Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #client{socket_type = SocketType, socket = Socket}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Binary}, State) ->
    %% send tcp/http(ws) binary
    sender:send(State, Binary),
    {noreply, State};
handle_cast({stop, Binary}, State) ->
    %% stop and send stop reason to client
    sender:send(State, Binary),
    %% stop this receiver
    {stop, normal, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(start_receive, State) ->
    %% start receive
    start_receive(?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, State#client{state = wait_pack_first});
handle_info({inet_async, Socket, Ref, {ok, Data}}, State = #client{socket = Socket, reference = Ref}) ->
    %% main receive & handle tpc data
    case reader:handle(State, Data) of
        {continue, NewState} ->
            {noreply, NewState};
        {read, Length, Timeout, NewState} ->
            start_receive(Length, Timeout, NewState);
        {stop, Reason, NewState} ->
            %% other reason is exception, need to report to error log
            {stop, Reason, NewState}
    end;
handle_info({inet_async, Socket, Ref, {error, Reason}}, State = #client{socket = Socket, reference = Ref}) ->
    %% tcp timeout/closed
    {stop, {shutdown, Reason}, State};
handle_info({inet_async, _Socket, Ref, Msg}, State = #client{reference = Ref}) ->
    %% ref not match
    {stop, {shutdown, Msg}, State};
handle_info({inet_async, _Socket, _Ref, Msg}, State) ->
    %% ref not match
    {stop, {shutdown, {ref_not_match, Msg}}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State = #client{socket_type = SocketType, socket = Socket, role_pid = RolePid}) ->
    %% report error
    gen_server:cast(RolePid, {disconnect, Reason}),
    %% close socket
    catch SocketType:close(Socket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
%% receive data
start_receive(Length, Timeout, State = #client{socket = Socket, socket_type = gen_tcp}) ->
    case prim_inet:async_recv(Socket, Length, Timeout) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
start_receive(Length, Timeout, State = #client{socket = Socket, socket_type = ssl}) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> erlang:send(Pid, {inet_async, Socket, Ref, catch ssl:recv(Socket, Length, Timeout)}) end),
    {noreply, State#client{reference = Ref}}.
