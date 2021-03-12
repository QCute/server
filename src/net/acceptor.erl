%%%-------------------------------------------------------------------
%%% @doc
%%% tcp acceptor
%%% @end
%%%-------------------------------------------------------------------
-module(acceptor).
-behaviour(gen_server).
%% API
-export([start/3, start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include("journal.hrl").
%% state
-record(state, {socket_type, listen_socket, reference, number = 0, increment = 0}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, ListenSocket :: gen_tcp:socket() | ssl:sslsocket(), Number :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(SocketType, ListenSocket, Number) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType, ListenSocket, Number]}, permanent, 60000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl, ListenSocket :: inet:socket(), Number :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(Name, SocketType, ListenSocket, Number) ->
    gen_server:start_link({local, Name}, ?MODULE, [SocketType, ListenSocket, Number], []).
%%%===================================================================
%%% gen_server callback
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([SocketType, ListenSocket, Number]) ->
    erlang:process_flag(trap_exit, true),
    %% start accept
    erlang:send(self(), start_accept),
    {ok, #state{socket_type = SocketType, listen_socket = ListenSocket, number = Number}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, Reply :: term(), NewState :: #state{}}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} | {stop, Reason :: term(), NewState :: #state{}}.
handle_info(start_accept, State) ->
    start_accept(State);
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{reference = Reference, listen_socket = #sslsocket{pid = {ListenSocket, _}}}) ->
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos, ttl, recvtos, recvttl]) of
        {ok, Opts} ->
            case prim_inet:setopts(Socket, Opts) of
                ok ->
                    inet_db:register_socket(Socket, inet_tcp),
                    ssl_start(Socket, State);
                {error, Reason} ->
                    prim_inet:close(Socket),
                    {stop, {setopts, Reason}, State}
            end;
        {error, Reason} ->
            prim_inet:close(Socket),
            {stop, {getopts, Reason}, State}
    end;
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{reference = Reference, listen_socket = ListenSocket}) ->
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos, ttl, recvtos, recvttl]) of
        {ok, Opts} ->
            case prim_inet:setopts(Socket, Opts) of
                ok ->
                    inet_db:register_socket(Socket, inet_tcp),
                    start_receiver(Socket, State);
                {error, Reason} ->
                    prim_inet:close(Socket),
                    {stop, {setopts, Reason}, State}
            end;
        {error, Reason} ->
            prim_inet:close(Socket),
            {stop, {getopts, Reason}, State}
    end;
handle_info({inet_async, _, _, Reason}, State) ->
    %% error state
    ?PRINT("Acceptor Error: ~w~n", [Reason]),
    {stop, normal, State};
handle_info(Info, State) ->
    ?PRINT("Unknown Acceptor Message: ~w~n", [Info]),
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> {ok, NewState :: #state{}}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% accept socket
start_accept(State = #state{listen_socket = #sslsocket{pid = {ListenSocket, _}}}) ->
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, Reference} ->
            {noreply, State#state{reference = Reference}};
        {error, Reason} ->
            %% accept error force close
            {stop, {async_accept, Reason}, State}
    end;
start_accept(State = #state{listen_socket = ListenSocket}) ->
    %% gen tcp async
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, Reference} ->
            {noreply, State#state{reference = Reference}};
        {error, Reason} ->
            %% accept error force close
            {stop, {async_accept, Reason}, State}
    end.

%% ssl start
ssl_start(Socket, State = #state{listen_socket = #sslsocket{pid = {_, #config{transport_info = {Transport, _, _, _} = CbInfo, connection_cb = ConnectionCb, ssl = SslOpts, emulated = Tracker}}}}) ->
    {ok, Port} = inet:port(Socket),
    {ok, Sender} = tls_sender:start(),
    ConnArgs = [server, Sender, "localhost", Port, Socket, {SslOpts, #socket_options{mode = binary, active = false}, Tracker}, self(), CbInfo],
    case tls_connection_sup:start_child(ConnArgs) of
        {ok, Pid} ->
            inet:tcp_controlling_process(Socket, Pid),
            case ssl_connection:handshake(#sslsocket{pid = [Pid, Sender], fd = {Transport, Socket, ConnectionCb, Tracker}}, 5000) of
                {ok, SSLSocket} ->
                    start_receiver(SSLSocket, State);
                {error, Reason} ->
                    {stop, {handshake, Reason}, State}
            end;
        {error, Reason} ->
            prim_inet:close(Socket),
            {stop, {start_child, Reason}, State}
    end.

%% start receiver process
start_receiver(Socket, State = #state{socket_type = SocketType, increment = Increment}) ->
    case receiver:start(SocketType, Socket) of
        {ok, Receiver} ->
            controlling_process(Socket, Receiver, State#state{increment = Increment + 1});
        {error, Reason} ->
            catch SocketType:close(Socket),
            {stop, {start_receiver, Reason}, State}
    end.

%% control the socket message send to receiver process
controlling_process(Socket, Receiver, State = #state{socket_type = SocketType}) ->
    case SocketType:controlling_process(Socket, Receiver) of
        ok ->
            %% start receive after controlling process succeeded
            erlang:send(Receiver, start_receive),
            %% next accept
            start_accept(State);
        {error, Reason} ->
            catch SocketType:close(Socket),
            %% stop receiver
            gen_server:stop(Receiver, normal, 5000),
            %% stop acceptor
            {stop, {controlling_process, Reason}, State}
    end.
