%%%-------------------------------------------------------------------
%%% @doc
%%% tcp sender
%%% @end
%%%-------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2]}).
%% API
-export([send/2, send/4]).
-export([send_ping/1, send_pong/1]).
%% Includes
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc send
-spec send(State :: #client{}, Data :: binary()) -> ok | {error, term()}.
send(#client{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}, Data) ->
    send(SocketType, Socket, ProtocolType, Data).

%% @doc send
-spec send(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), ProtocolType :: tcp | web_socket, Data :: binary()) -> ok | {error, term()}.
send(SocketType, Socket, tcp, Data) ->
    send_data(SocketType, Socket, Data);
send(SocketType, Socket, web_socket, Data) ->
    send_data(SocketType, Socket, pack_with_length(byte_size(Data), Data)).

%% @doc send data
-spec send_data(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), Data :: binary()) -> ok | {error, term()}.
send_data(gen_tcp, Socket, Data) ->
    gen_tcp:send(Socket, Data);
send_data(ssl, Socket, Data) ->
    ssl:send(Socket, Data).

%% web socket packet
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 2:4>> = <<130>>.
pack_with_length(Length, Data) when Length =< 125 ->
    <<130, Length:8, Data/binary>>;
pack_with_length(Length, Data) when Length =< 65535 ->
    <<130, 126, Length:16, Data/binary>>;
pack_with_length(Length, Data) ->
    <<130, 127, Length:64, Data/binary>>.

%% @doc send web socket ping frame
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 9:4>> = <<137>>.
-spec send_ping(State :: #client{}) -> ok | {error, term()}.
send_ping(#client{socket_type = SocketType, socket = Socket}) ->
    send_data(SocketType, Socket, <<137, 127, 9:8>>).

%% @doc send web socket pong frame
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 10:4>> = <<138>>.
-spec send_pong(State :: #client{}) -> ok | {error, term()}.
send_pong(#client{socket_type = SocketType, socket = Socket}) ->
    send_data(SocketType, Socket, <<138, 127, 10:8>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================
