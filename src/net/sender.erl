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
-include("socket.hrl").
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
send(ssl, Socket, Binary) ->
    ssl:send(Socket, Binary).

%% @doc send
-spec send(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:socket(), ProtocolType :: tcp | 'HyBi' | 'HiXie', Binary :: binary()) -> term().
send(SocketType, Socket, tcp, Binary) ->
    send(SocketType, Socket, Binary);
send(SocketType, Socket, 'HyBi', Binary) ->
    Length = byte_size(Binary),
    case Length < 126 of
        true ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, Length:7, Binary/binary>>;
        _ when Length < 65535 ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, 126:7, Length:16, Binary/binary>>;
        _ ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, 127:7, Length:64, Binary/binary>>
    end,
    send(SocketType, Socket, NewBinary);
send(SocketType, Socket, 'HiXie', Binary) ->
    send(SocketType, Socket, <<0:8, Binary/binary, 255:8>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================
