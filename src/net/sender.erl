%%%-------------------------------------------------------------------
%%% @doc
%%% module sender
%%% @end
%%%-------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2]}).
%% API
-export([send/2, send/3, send/4]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc send
-spec send(State :: #client{}, Binary :: binary()) -> term().
send(#client{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}, Binary) ->
    send(SocketType, Socket, ProtocolType, Binary).

%% @doc send
-spec send(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:socket(), Binary :: binary()) -> term().
send(gen_tcp, Socket, Binary) ->
    erts_internal:port_command(Socket, Binary, [force]);
send(ssl, #sslsocket{pid = [_, Pid]}, Binary) ->
    erlang:send(Pid, {'$gen_call', {self(), 0}, {application_data, erlang:iolist_to_iovec(Binary)}}).

%% @doc send
-spec send(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:socket(), ProtocolType :: tcp | web_socket, Binary :: binary()) -> term().
send(SocketType, Socket, tcp, Binary) ->
    send(SocketType, Socket, Binary);
send(SocketType, Socket, web_socket, Binary) ->
    Length = byte_size(Binary),
    send(SocketType, Socket, pack_with_length(Length, Binary)).

%% web socket packet
pack_with_length(Length, Binary) when Length =< 125 ->
    <<130, Length:8, Binary/binary>>;
pack_with_length(Length, Binary) when Length =< 16#FFFF ->
    <<130, 126, Length:16, Binary/binary>>;
pack_with_length(Length, Binary) ->
    <<130, 127, Length:64, Binary/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
