%%%-------------------------------------------------------------------
%%% @doc
%%% tcp acceptor
%%% @end
%%%-------------------------------------------------------------------
-module(acceptor).
%% API
-export([start/3, start_link/3]).
-export([loop/2]).
%% Includes
-include("journal.hrl").
-include("time.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, ListenSocket :: gen_tcp:socket() | ssl:sslsocket(), Number :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(SocketType, ListenSocket, Number) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType, ListenSocket]}, permanent, 60000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl, ListenSocket :: gen_tcp:socket() | ssl:sslsocket()) -> {ok, pid()}.
start_link(Name, SocketType, ListenSocket) ->
    Pid = spawn_link(?MODULE, loop, [SocketType, ListenSocket]),
    erlang:register(Name, Pid),
    {ok, Pid}.

%% @doc loop
-spec loop(SocketType :: gen_tcp | ssl, ListenSocket :: gen_tcp:socket() | ssl:sslsocket()) -> no_return().
loop(SocketType = gen_tcp, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Receiver = receiver:start(SocketType, Socket),
            controlling_process(SocketType, Socket, Receiver),
            %% loop
            loop(SocketType, ListenSocket);
        {error, closed} ->
            exit(normal);
        {error, Reason} ->
            ?PRINT("Acceptor Error: ~p in gen_tcp:accept", [Reason])
    end;
loop(SocketType = ssl, ListenSocket) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            %% ssl handshake
            case ssl:handshake(Socket, ?SECOND_MILLISECONDS(10)) of
                {ok, SslSocket} ->
                    Receiver = receiver:start(SocketType, SslSocket),
                    controlling_process(SocketType, SslSocket, Receiver);
                {ok, SslSocket, Ext} ->
                    ?PRINT("Acceptor Notice: ProtocolExtensions: ~p in ssl:handshake", [Ext]),
                    Receiver = receiver:start(SocketType, SslSocket),
                    controlling_process(SocketType, SslSocket, Receiver);
                {error, Reason} ->
                    ?PRINT("Acceptor Error: ~p in ssl:handshake", [Reason])
            end,
            %% loop
            loop(SocketType, ListenSocket);
        {error, closed} ->
            exit(normal);
        {error, Reason} ->
            ?PRINT("Acceptor Error: ~p in ssl:transport_accept", [Reason])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% controlling_process
controlling_process(gen_tcp, Socket, Receiver) ->
    case gen_tcp:controlling_process(Socket, Receiver) of
        ok ->
            %% start receive after controlling process succeeded
            erlang:send(Receiver, start);
        {error, Reason} ->
            ?PRINT("Acceptor Error: ~p in gen_tcp:controlling_process", [Reason]),
            %% close socket
            gen_tcp:close(Socket)
    end;
controlling_process(ssl, Socket, Receiver) ->
    case ssl:controlling_process(Socket, Receiver) of
        ok ->
            %% start receive after controlling process succeeded
            erlang:send(Receiver, start);
        {error, Reason} ->
            ?PRINT("Acceptor Error: ~p in ssl:controlling_process", [Reason]),
            %% close socket
            ssl:close(Socket)
    end.
