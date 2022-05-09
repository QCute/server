%%%-------------------------------------------------------------------
%%% @doc
%%% tcp receiver
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-compile({inline, [web_socket_length/3, web_socket_unmask/5, unmask/2]}).
-compile({inline, [receive_data/2]}).
%% API
-export([start/2]).
-export([init/1]).
-export([close/2]).
%% Includes
-include("time.hrl").
-include("journal.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> pid().
start(SocketType = gen_tcp, Socket) ->
    {ok, {IP, _Port}} = inet:peername(Socket),
    %% do not mirror by the net supervisor
    spawn(?MODULE, init, [#client{socket_type = SocketType, socket = Socket, ip = IP}]);
start(SocketType = ssl, Socket) ->
    {ok, {IP, _Port}} = ssl:peername(Socket),
    %% do not mirror by the net supervisor
    spawn(?MODULE, init, [#client{socket_type = SocketType, socket = Socket, ip = IP}]).

%% handle tcp/http stream
-spec init(State :: #client{}) -> ok | no_return().
init(State) ->
    %% wait for start
    _ = receive start -> ok after ?SECOND_MILLISECONDS(10) -> ok end,
    try
        %% start receive
        case receive_data(State, 0) of
            Data = <<"GET", 32, _/binary>> ->
                %% Conflict Length:18245 Protocol:21536
                decode_http(State, byte_size(Data), Data, [<<>>]);
            Data = <<"POST", _/binary>> ->
                %% Conflict Length:20559 Protocol:21332
                decode_http(State, byte_size(Data), Data, [<<>>]);
            Data = <<"HEAD", _/binary>> ->
                %% Conflict Length:18501 Protocol:16708
                decode_http(State, byte_size(Data), Data, [<<>>]);
            <<"TCP", 32, Data/binary>> ->
                %% Conflict Length:21571 Protocol:20512
                stream_loop(State, Data);
            Data ->
                stream_loop(State, Data)
        end
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        Reason =/= normal andalso ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc close socket
-spec close(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> ok | {error, term()}.
close(gen_tcp, Socket) ->
    gen_tcp:close(Socket);
close(ssl, Socket) ->
    ssl:close(Socket).

%%%===================================================================
%%% tcp
%%%===================================================================
%% stream loop
stream_loop(State, <<Length:16, Protocol:16, Binary:Length/binary, Rest/binary>>) ->
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(State, Protocol, Data) of
                {ok, NewState} ->
                    %% continue
                    stream_loop(NewState, Rest);
                {stop, Reason, NewState} ->
                    gen_server:cast(State#client.role_pid, {disconnect, Reason}),
                    close(NewState#client.socket_type, NewState#client.socket),
                    exit(Reason)
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [Length, Protocol, Binary]),
            %% continue
            stream_loop(State, Rest)
    end;
stream_loop(State, Stream) ->
    Data = receive_data(State, 0),
    stream_loop(State, <<Stream/binary, Data/binary>>).

%%%===================================================================
%%% http Request parse
%%%===================================================================
%% Method[Space]URI[Space]Version[CR,LF]
%% Name:[Maybe Space]Value[CR,LF]
%% ...
%% Name:[Maybe Space]Value[CR,LF]
%% [CR,LF]
%% body

%% decode http packet
decode_http(State, Length, <<"\r\n", Rest/binary>>, [Version, URI, Method]) ->
    %% completed row header, decode header field
    decode_http_header(State, #http{method = Method, uri = URI, version = Version}, Length, Rest, [key, <<>>]);
decode_http(State, Length, <<32, Rest/binary>>, Result) ->
    %% space separator
    decode_http(State, Length, Rest, [<<>> | Result]);
decode_http(State, Length, <<Byte, Rest/binary>>, [Segment | Result]) ->
    %% segment
    decode_http(State, Length, Rest, [<<Segment/binary, Byte>> | Result]);
decode_http(State, Length, Stream, Result) ->
    %% http header size 1k
    case Length =< 1024 of
        true ->
            Data = receive_data(State, 0),
            decode_http(State, Length + byte_size(Data), <<Stream/binary, Data/binary>>, Result);
        false ->
            %% body size out of limit
            ?PRINT("Http Header Length: ~p Out of Limit: ~tp", [Length, Result])
    end.

%% decode http header
decode_http_header(State, Http, Length, <<"\r\n", Rest/binary>>, [key, <<>> | Result]) ->
    Value = find_header_value(<<"Content-Length">>, Result, <<"0">>),
    %% parse content length
    ContentLength = try
        binary_to_integer(Value)
    catch _:_ ->
        ?PRINT("Invalid Content-Length: ~p in Http Request Header", [Value]),
        close(State#client.socket_type, State#client.socket),
        exit(normal)
    end,
    case Rest of
        <<Body:ContentLength/binary, Remain/binary>> ->
            %% complete packet, with body
            handle_http_request(State, Http#http{fields = Result}, Body, Remain);
        _ ->
            %% http body size limit 64k
            case ContentLength =< 65536 of
                true ->
                    %% incomplete packet, continue
                    decode_http_header(State, Http, Length, Rest, Result);
                _ ->
                    %% body size out of limit
                    ?PRINT("Http Content Length: ~p Out of Limit: ~tp", [ContentLength, Http#http{fields = Result}])
            end
    end;
decode_http_header(State, Http, Length, <<"\r\n", Rest/binary>>, [value, Value, key, Key | Result]) ->
    %% key value pair
    decode_http_header(State, Http, Length, Rest, [key, <<>>, {Key, Value} | Result]);
decode_http_header(State, Http, Length, <<":", Rest/binary>>, [key | Result]) ->
    %% key value separator
    decode_http_header(State, Http, Length, Rest, [value, <<>>, key | Result]);
decode_http_header(State, Http, Length, <<32, Rest/binary>>, [key, <<>> | Result]) ->
    %% space
    decode_http_header(State, Http, Length, Rest, [key, <<>> | Result]);
decode_http_header(State, Http, Length, <<32, Rest/binary>>, [value, <<>> | Result]) ->
    %% space
    decode_http_header(State, Http, Length, Rest, [value, <<>> | Result]);
decode_http_header(State, Http, Length, <<Byte, Rest/binary>>, [key, Segment | Result]) ->
    %% key
    decode_http_header(State, Http, Length, Rest, [key, <<Segment/binary, Byte>> | Result]);
decode_http_header(State, Http, Length, <<Byte, Rest/binary>>, [value, Segment | Result]) ->
    %% value
    decode_http_header(State, Http, Length, Rest, [value, <<Segment/binary, Byte>> | Result]);
decode_http_header(State, Http, Length, Stream, Result) ->
    %% http header size 1k
    case Length =< 1024 of
        true ->
            %% incomplete packet, continue
            Data = receive_data(State, 0),
            decode_http_header(State, Http, Length + byte_size(Data), <<Stream/binary, Data/binary>>, Result);
        false ->
            %% body size out of limit
            ?PRINT("Http Header Length: ~p Out of Limit: ~tp", [Length, Http#http{fields = Result}])
    end.

%% key
find_header_value(_, [], Default) ->
    Default;
find_header_value(Key, [{Key, Value} | _], _) ->
    Value;
find_header_value(Name, [{Key, Value} | T], Default) ->
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Key>> of
        Name ->
            Value;
        _ ->
            find_header_value(Name, T, Default)
    end.

%%%===================================================================
%%% http
%%%===================================================================
%% Method Support
%% HTTP/1.0
%% GET, POST, HEAD
%% HTTP/1.1
%% OPTIONS, PUT, DELETE, TRACE, CONNECT

%% http request
handle_http_request(State, #http{method = <<"HEAD">>, version = Version}, _, Data) ->
    Response = [
        Version, <<" 200 OK\r\n">>,
        <<"Connection: keep-alive\r\n">>,
        <<"Keep-Alive: timeout=60, max=1000\r\n">>,
        <<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>,
        <<"Server: ">>, <<"erlang/">>, erlang:system_info(version), <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Response)),
    decode_http(State, byte_size(Data), Data, [<<>>]);
handle_http_request(State, Http = #http{fields = Fields}, Body, Data) ->
    Upgrade = find_header_value(<<"Upgrade">>, Fields, <<"">>),
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Upgrade>> of
        <<"websocket">> ->
            %% http upgrade (WebSocket)
            web_socket_handshake(State, Http, Upgrade),
            %% enter web socket stream loop
            Stream = receive_data(State, 0),
            web_socket_header(State#client{protocol_type = web_socket}, Stream, <<>>);
        <<"tcp">> ->
            %% enter tcp stream loop
            stream_loop(State, Data);
        _ ->
            %% normal http Request
            case master:treat(State, Http, Body) of
                {ok, NewState} ->
                    decode_http(NewState, byte_size(Data), Data, [<<>>]);
                {stop, Reason, NewState} ->
                    close(NewState#client.socket_type, NewState#client.socket),
                    exit(Reason)
            end
    end.

%%%===================================================================
%%% http upgrade
%%%===================================================================
%% web socket handshake
web_socket_handshake(State, #http{version = Version, fields = Fields}, Upgrade) ->
    SecKey = find_header_value(<<"Sec-WebSocket-Key">>, Fields, <<"">>),
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        Version, <<" 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)).

%%%===================================================================
%%% Web Socket Draft-HyBi-10-17 Exchanging Data Frames
%%%===================================================================

%% Header
%% +---------+--------+------------------------+
%% | FIN     | 1      | End(1)/Continuation(0) |
%% +---------+--------+------------------------+
%% | RSV1    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | RSV2    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | RSV3    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | OpCode  | 4      | OpCode                 |
%% +---------+--------+------------------------+
%% | Mask    | 1      | Mask Flag              |
%% +---------+--------+------------------------+
%% |         | 7      | =< 125                 |
%% | Length  | 7 + 16 | 126                    |
%% |         | 7 + 64 | 127                    |
%% +---------+--------+------------------------+
%% | Masking | 0/32   | Mask(0/1)              |
%% +---------+--------+------------------------+
%% | Payload | Length | xor with Masking       |
%% +---------+--------+------------------------+

%% OpCode
%% +---------+---------------------------------+
%% |    0    |  Continuation Frame             |
%% +---------+---------------------------------+
%% |    1    |  Text Frame                     |
%% +---------+---------------------------------+
%% |    2    |  Binary Frame                   |
%% +---------+---------------------------------+
%% |   3-7   |  Reserve                        |
%% +---------+---------------------------------+
%% |    8    |  Connection Close Frame         |
%% +---------+---------------------------------+
%% |    9    |  Ping Frame                     |
%% +---------+---------------------------------+
%% |   10    |  Pong Frame                     |
%% +---------+---------------------------------+
%% |  11-15  |  Reserve                        |
%% +---------+---------------------------------+

%% handle web socket header
web_socket_header(State, <<_Fin:1, _Rsv:3, 1:4, Rest/binary>>, Packet) ->
    %% text packet
    web_socket_length(State, Rest, Packet);
web_socket_header(State, <<_Fin:1, _Rsv:3, 2:4, Rest/binary>>, Packet) ->
    %% binary packet
    web_socket_length(State, Rest, Packet);
web_socket_header(State, <<_Fin:1, _Rsv:3, 9:4, Rest/binary>>, Packet) ->
    %% ping => pong
    sender:send_pong(State),
    %% packet
    web_socket_length(State, Rest, Packet);
web_socket_header(State, <<_Fin:1, _Rsv:3, _:4, _/binary>>, _Packet) ->
    %% close/unknown
    gen_server:cast(State#client.role_pid, {disconnect, normal}),
    close(State#client.socket_type, State#client.socket),
    exit(normal);
web_socket_header(State, Stream, Packet) ->
    %% incomplete header
    Data = receive_data(State, 0),
    web_socket_header(State, <<Stream/binary, Data/binary>>, Packet).

%% handle web socket body length
web_socket_length(State, <<Mask:1, Length:7, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) when Length =< 125 ->
    web_socket_unmask(State, Length, Masking, Rest, Packet);
web_socket_length(State, <<Mask:1, 126:7, Length:16, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    web_socket_unmask(State, Length, Masking, Rest, Packet);
web_socket_length(State, <<Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    web_socket_unmask(State, Length, Masking, Rest, Packet);
web_socket_length(State, Stream, Packet) ->
    %% incomplete header length
    Data = receive_data(State, 0),
    web_socket_length(State, <<Stream/binary, Data/binary>>, Packet).

%% web socket body unmask
web_socket_unmask(State, Length, Masking, Rest, Packet) ->
    case Rest of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            {_, Data} = unmask(Body, Masking),
            web_socket_loop(State, 0, <<>>, RestStream, <<Packet/binary, Data/binary>>);
        _ ->
            %% incomplete packet
            {NewMasking, Data} = unmask(Rest, Masking),
            web_socket_loop(State, Length - byte_size(Rest), NewMasking, <<>>, <<Packet/binary, Data/binary>>)
    end.

%% web socket protocol dispatch
web_socket_loop(State, Length, Masking, Stream, <<PacketLength:16, Protocol:16, Binary:PacketLength/binary, Rest/binary>>) ->
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(State, Protocol, Data) of
                {ok, NewState} ->
                    %% continue
                    web_socket_loop(NewState, Length, Masking, Stream, Rest);
                {stop, Reason, NewState} ->
                    gen_server:cast(NewState#client.role_pid, {disconnect, Reason}),
                    close(NewState#client.socket_type, NewState#client.socket),
                    exit(Reason)
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [Length, Protocol, Binary]),
            %% continue
            web_socket_loop(State, Length, Masking, Stream, Rest)
    end;
web_socket_loop(State, 0, _Masking, Stream, Packet) ->
    %% next packet
    web_socket_header(State, Stream, Packet);
web_socket_loop(State, Length, Masking, Stream, Packet) ->
    %% continue
    Data = receive_data(State, 0),
    web_socket_unmask(State, Length, Masking, <<Stream/binary, Data/binary>>, Packet).

%%%===================================================================
%%% web socket frame decode
%%%===================================================================
%% unmask
unmask(Payload, <<>>) ->
    Payload;
unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).

%% unmask body
unmask(<<Payload:32, Rest/binary>>, Masking = <<Mask:32>>, Acc) ->
    unmask(Rest, Masking, <<Acc/binary, (Payload bxor Mask):32>>);
unmask(<<Payload:24>>, <<Mask:24, Rest:8>>, Acc) ->
    {<<Rest:8, Mask:24>>, <<Acc/binary, (Payload bxor Mask):24>>};
unmask(<<Payload:16>>, <<Mask:16, Rest:16>>, Acc) ->
    {<<Rest:16, Mask:16>>, <<Acc/binary, (Payload bxor Mask):16>>};
unmask(<<Payload:8>>, <<Mask:8, Rest:24>>, Acc) ->
    {<<Rest:24, Mask:8>>, <<Acc/binary, (Payload bxor Mask):8>>};
unmask(<<>>, Masking, Acc) ->
    {Masking, Acc}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive data
receive_data(#client{socket_type = SocketType, socket = Socket, role_pid = RolePid}, Length) ->
    case SocketType:recv(Socket, Length, ?MINUTE_MILLISECONDS) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            gen_server:cast(RolePid, {disconnect, normal}),
            exit(normal);
        {error, timeout} ->
            gen_server:cast(RolePid, {disconnect, normal}),
            exit(normal);
        {error, Reason} ->
            gen_server:cast(RolePid, {disconnect, Reason}),
            exit(Reason)
    end.
