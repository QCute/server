%%%------------------------------------------------------------------
%%% @doc
%%% module acceptor
%%% @end
%%%------------------------------------------------------------------
-module(acceptor).
-behaviour(gen_server).
%% API
-export([start/3, start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state
-record(state, {socket_type, listen_socket, reference, number = 0, increment = 1}).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, ListenSocket :: inet:socket(), Number :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(SocketType, ListenSocket, Number) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType, ListenSocket, Number]}, permanent, 10000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl, ListenSocket :: inet:socket(), Number :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(Name, SocketType, ListenSocket, Number) ->
    gen_server:start_link({local, Name}, ?MODULE, [SocketType, ListenSocket, Number], []).
%%%==================================================================
%%% gen_server callback
%%%==================================================================
init([SocketType, ListenSocket, Number]) ->
    erlang:process_flag(trap_exit, true),
    %% start accept
    erlang:send(self(), async_accept),
    {ok, #state{socket_type = SocketType, listen_socket = ListenSocket, number = Number}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(async_accept, State) ->
    async_accept(State);
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = gen_tcp, reference = Reference, listen_socket = ListenSocket}) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, keepalive, priority, tos]) of
        {ok, Options} ->
            case prim_inet:setopts(ListenSocket, Options) of
                ok ->
                    start_receiver(Socket, State);
                {error, Reason} ->
                    catch gen_tcp:close(Socket),
                    {stop, {inet_setopts, Reason}, State}
            end;
        {error, Reason} ->
            catch gen_tcp:close(Socket),
            {stop, {inet_getopts, Reason}, State}
    end;
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = ssl, reference = Reference, listen_socket = ListenSocket}) ->
    %% before ssl:ssl_accept()
    %% current ssl:handshake()
    case ssl:handshake(Socket) of
        {ok, SSLSocket} ->
            case ssl:setopts(SSLSocket, [{packet, 0}, {active, false}, {keepalive, false}]) of
                ok ->
                    start_receiver(SSLSocket, State);
                {error, Reason} ->
                    catch ssl:close(SSLSocket),
                    {stop, {ssl_setopts, Reason}, State}
            end;
        {error, Reason} ->
            catch ssl:close(Socket),
            {stop, {ssl_handshake, Reason}, State}
    end;
handle_info({inet_async, _, _, {error, closed}}, State) ->
    %% error state
    {noreply, State};
handle_info({inet_async, _, _, Reason}, State) ->
    %% error state
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
%% accept socket
async_accept(State = #state{socket_type = gen_tcp, listen_socket = ListenSocket}) ->
    %% gen tcp async
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, Reference} ->
            {noreply, State#state{reference = Reference}};
        {error, Reason} ->
            %% accept error force close
            {stop, {async_accept, Reason}, State}
    end;
async_accept(State = #state{socket_type = ssl, listen_socket = ListenSocket}) ->
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
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, {ok, Socket}});
                {error, Reason} ->
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, {controlling_process, Reason}})
            end;
        {error, Reason} ->
            erlang:send(Pid, {inet_async, ListenSocket, Reference, {transport_accept, Reason}})
    end.

%% start receiver process
start_receiver(Socket, State = #state{socket_type = SocketType, increment = Increment, number = Number}) ->
    case receiver:start(SocketType, Socket, Number, Increment) of
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
            async_accept(State);
        {error, Reason} ->
            catch SocketType:close(Socket),
            %% stop receiver
            gen_server:stop(Receiver, normal, 5000),
            %% stop acceptor
            {stop, {controlling_process, Reason}, State}
    end.
