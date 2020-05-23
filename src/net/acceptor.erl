%%%-------------------------------------------------------------------
%%% @doc
%%% module acceptor
%%% @end
%%%-------------------------------------------------------------------
-module(acceptor).
-behaviour(gen_server).
%% API
-export([start/3, start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
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
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = gen_tcp, reference = Reference, listen_socket = ListenSocket}) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    start_receiver(Socket, State);
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = ssl, reference = Reference, listen_socket = ListenSocket}) ->
    start_receiver(Socket, State);
handle_info({inet_async, _, _, {error, closed}}, State) ->
    %% error state
    {noreply, State};
handle_info({inet_async, _, _, Reason}, State) ->
    %% error state
    {stop, Reason, State};
handle_info(_Info, State) ->
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
start_accept(State = #state{socket_type = gen_tcp, listen_socket = ListenSocket}) ->
    %% gen tcp async
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, Reference} ->
            {noreply, State#state{reference = Reference}};
        {error, Reason} ->
            %% accept error force close
            {stop, {async_accept, Reason}, State}
    end;
start_accept(State = #state{socket_type = ssl, listen_socket = ListenSocket}) ->
    %% ssl
    Pid = self(),
    Reference = make_ref(),
    %% async accept
    spawn(fun() -> transport_accept(Pid, ListenSocket, Reference) end),
    {noreply, State#state{reference = Reference}}.

%% async accept for ssl
transport_accept(Pid, ListenSocket, Reference) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case ssl:controlling_process(Socket, Pid) of
                ok ->
                    %% before ssl:ssl_accept()
                    %% current ssl:handshake()
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, ssl:handshake(Socket)});
                {error, Reason} ->
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, {controlling_process, Reason}})
            end;
        {error, Reason} ->
            erlang:send(Pid, {inet_async, ListenSocket, Reference, {transport_accept, Reason}})
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
