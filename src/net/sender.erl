%%%-------------------------------------------------------------------
%%% @doc
%%% tcp sender
%%% @end
%%%-------------------------------------------------------------------
-module(sender).
-compile({no_auto_import, [send/2]}).
%% API
-export([send/2, send/3]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc send
-spec send(State :: #client{}, Binary :: binary()) -> term().
send(#client{socket = Socket, protocol_type = ProtocolType}, Binary) ->
    send(Socket, ProtocolType, Binary).

%% @doc send
-spec send_binary(Socket :: gen_tcp:socket() | ssl:sslsocket(), Binary :: binary()) -> term().
send_binary(#sslsocket{pid = [_, Pid]}, Binary) ->
    erlang:send(Pid, {'$gen_call', {self(), 0}, {application_data, erlang:iolist_to_iovec(Binary)}});
send_binary(Socket, Binary) ->
    erts_internal:port_command(Socket, Binary, [force]).

%% @doc send
-spec send(Socket :: gen_tcp:socket() | ssl:sslsocket(), ProtocolType :: tcp | web_socket, Binary :: binary()) -> term().
send(Socket, tcp, Binary) ->
    send_binary(Socket, Binary);
send(Socket, web_socket, Binary) ->
    Length = byte_size(Binary),
    send_binary(Socket, pack_with_length(Length, Binary)).

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
