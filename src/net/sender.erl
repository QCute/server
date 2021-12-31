%%%-------------------------------------------------------------------
%%% @doc
%%% tcp sender
%%% @end
%%%-------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2]}).
%% API
-export([send/2, send/4]).
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
pack_with_length(Length, Data) when Length =< 125 ->
    <<130, Length:8, Data/binary>>;
pack_with_length(Length, Data) when Length =< 65535 ->
    <<130, 126, Length:16, Data/binary>>;
pack_with_length(Length, Data) ->
    <<130, 127, Length:64, Data/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
