%%%------------------------------------------------------------------
%%% @doc
%%% module sender
%%% @end
%%%------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2, send/3, send/4]}).
%% API
-export([response/2, send/2, send/4]).
%% Includes
-include("socket.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc response
-spec response(State :: #client{}, Binary :: binary()) -> ok.
response(#client{socket = Socket, socket_type = gen_tcp}, Binary) ->
    erts_internal:port_command(Socket, Binary, [force]);
response(#client{socket = Socket, socket_type = ssl}, Binary) ->
    ssl:send(Socket, Binary).

%% @doc send
-spec send(State :: #client{}, Binary :: binary()) -> ok.
send(#client{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}, Binary) ->
    send(Socket, SocketType, ProtocolType, Binary).

%% @doc send
-spec send(Socket :: port(), SocketType :: gen_tcp | ssl, ProtocolType :: tcp | 'HyBi' | 'HiXie', Binary :: binary()) -> ok.
send(Socket, gen_tcp, 'HyBi', Binary) ->
    Length = byte_size(Binary),
    case Length < 126 of
        true ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, Length:7, Binary/binary>>;
        _ when Length < 65535 ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, 126:7, Length:16, Binary/binary>>;
        _ ->
            NewBinary = <<1:1, 0:3, 2:4, 0:1, 127:7, Length:64, Binary/binary>>
    end,
    erts_internal:port_command(Socket, NewBinary, [force]);
send(Socket, ssl, 'HiXie', Binary) ->
    ssl:send(Socket, <<0:8, Binary/binary, 255:8>>);
send(Socket, gen_tcp, tcp, Binary) ->
    erts_internal:port_command(Socket, Binary, [force]);
send(Socket, ssl, tcp, Binary) ->
    ssl:send(Socket, Binary).

%%%==================================================================
%%% Internal functions
%%%==================================================================
