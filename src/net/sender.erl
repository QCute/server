%%%-------------------------------------------------------------------
%%% @doc
%%% tcp sender
%%% @end
%%%-------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2, send/3]}).
-compile({inline, [send/4, send_data/3, pack_with_length/2]}).
%% API
-export([send/2, send/4]).
-export([send_close/2, send_ping/3, send_pong/3]).
%% Includes
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc send
-spec send(Client :: #client{}, Data :: binary()) -> ok | {error, term()}.
send(#client{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}, Data) ->
    send(SocketType, Socket, ProtocolType, Data).

%% @doc send
-spec send(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), ProtocolType :: tcp | web_socket, Data :: binary()) -> ok | {error, term()}.
send(SocketType, Socket, tcp, Data) ->
    send_data(SocketType, Socket, Data);
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 2:4>> = <<130>>.
send(SocketType, Socket, web_socket, Data) ->
    send_data(SocketType, Socket, pack_with_length(130, Data)).

%% @doc send data
-spec send_data(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), Data :: binary()) -> ok | {error, term()}.
send_data(SocketType, Socket, Data) ->
    SocketType:send(Socket, Data).

%% @doc send web socket close frame
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 8:4>> = <<136>>.
-spec send_close(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> ok | {error, term()}.
send_close(SocketType, Socket) ->
    send_data(SocketType, Socket, pack_with_length(136, <<>>)).

%% @doc send web socket ping frame
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 9:4>> = <<137>>.
-spec send_ping(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), Data :: binary()) -> ok | {error, term()}.
send_ping(SocketType, Socket, Data) ->
    send_data(SocketType, Socket, pack_with_length(137, Data)).

%% @doc send web socket pong frame
%% <<Fin = 1:1, Rsv = 0:3, Opcode = 10:4>> = <<138>>.
-spec send_pong(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), Data :: binary()) -> ok | {error, term()}.
send_pong(SocketType, Socket, Data) ->
    send_data(SocketType, Socket, pack_with_length(138, Data)).

%% @doc pack web socket packet
-spec pack_with_length(Type :: non_neg_integer(), Data :: binary()) -> binary().
pack_with_length(Type, Data) ->
    Length = byte_size(Data),
    case Length =< 125 of
        true ->
            <<Type:8, Length:8, Data/binary>>;
        false ->
            case Length =< 65535 of
                true ->
                    <<Type:8, 126, Length:16, Data/binary>>;
                false ->
                    <<Type:8, 127, Length:64, Data/binary>>
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
