%%%-------------------------------------------------------------------
%%% @doc
%%% tcp receiver
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-compile({inline, [web_socket_length/3, web_socket_body/5]}).
-compile({inline, [receive_data/2]}).
%% API
-export([start/2]).
-export([init/1]).
-export([close/3]).
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
-spec close(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket(), ProtocolType :: tcp | web_socket) -> ok | {error, term()}.
close(SocketType, Socket, tcp) ->
    sender:send_close(SocketType, Socket),
    SocketType:close(Socket);
close(SocketType, Socket, web_socket) ->
    sender:send_close(SocketType, Socket),
    SocketType:close(Socket).

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
                    close(NewState#client.socket_type, NewState#client.socket, NewState#client.protocol_type),
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
decode_http(State, Length, <<32, Rest/binary>>, [Version, URI, Method]) ->
    %% space separator
    decode_http(State, Length, Rest, [<<Version/binary, 32>>, URI, Method]);
decode_http(State, Length, <<32, Rest/binary>>, Result) ->
    %% space separator
    decode_http(State, Length, Rest, [<<>> | Result]);
decode_http(State, Length, <<Byte, Rest/binary>>, [Segment | Result]) ->
    %% segment
    decode_http(State, Length, Rest, [<<Segment/binary, Byte>> | Result]);
decode_http(State, Length, Stream, Result) ->
    %% http header size 64k
    case Length =< 65536 of
        true ->
            Data = receive_data(State, 0),
            decode_http(State, Length + byte_size(Data), <<Stream/binary, Data/binary>>, Result);
        false ->
            Response = <<
                "HTTP/1.1", " ", "413", " ", "Request Entity Too Large", "\r\n",
                "Connection", ":", "close", "\r\n",
                "\r\n"
            >>,
            sender:send(State, Response),
            %% close
            close(State#client.socket_type, State#client.socket, State#client.protocol_type),
            %% body size out of limit
            ?PRINT("Http Header Length: ~p Out of Limit: ~tp", [Length, Result])
    end.

%% decode http header
decode_http_header(State, Http, Length, <<"\r\n", Rest/binary>>, [key, <<>> | Result]) ->
    Value = listing:key_get(<<"content-length">>, 1, Result, <<"0">>),
    %% parse content length
    ContentLength = try
        binary_to_integer(Value)
    catch _:_ ->
        ?PRINT("Invalid Content-Length: ~p in Http Request Header", [Value]),
        BadRequestResponse = <<
            "HTTP/1.1", " ", "400", " ", "Bad Request", "\r\n",
            "Connection", ":", "close", "\r\n",
            "\r\n"
        >>,
        sender:send(State, BadRequestResponse),
        close(State#client.socket_type, State#client.socket, State#client.protocol_type),
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
                    Response = <<
                        "HTTP/1.1", " ", "413", " ", "Request Entity Too Large", "\r\n",
                        "Connection", ":", "close", "\r\n",
                        "\r\n"
                    >>,
                    sender:send(State, Response),
                    %% close
                    close(State#client.socket_type, State#client.socket, State#client.protocol_type),
                    %% body size out of limit
                    ?PRINT("Http Content Length: ~p Out of Limit: ~tp", [ContentLength, Http#http{fields = Result}])
            end
    end;
decode_http_header(State, Http, Length, <<"\r\n", Rest/binary>>, [value, Value, key, Key | Result]) ->
    %% key value pair
    decode_http_header(State, Http, Length, Rest, [key, <<>>, {<<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Key>>, Value} | Result]);
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
    %% http header size 64k
    case Length =< 65536 of
        true ->
            %% incomplete packet, continue
            Data = receive_data(State, 0),
            decode_http_header(State, Http, Length + byte_size(Data), <<Stream/binary, Data/binary>>, Result);
        false ->
            Response = <<
                "HTTP/1.1", " ", "413", " ", "Request Entity Too Large", "\r\n",
                "Connection", ":", "close", "\r\n",
                "\r\n"
            >>,
            sender:send(State, Response),
            %% close
            close(State#client.socket_type, State#client.socket, State#client.protocol_type),
            %% body size out of limit
            ?PRINT("Http Header Length: ~p Out of Limit: ~tp", [Length, Http#http{fields = Result}])
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
    Response = <<
        Version/binary, " ", "200", " ", "OK", "\r\n",
        "Connection", ":", "keep-alive", "\r\n",
        "Keep-Alive", ":", "timeout=60, max=1000", "\r\n",
        "Date", ":", (list_to_binary(httpd_util:rfc1123_date()))/binary, "\r\n",
        "Server", ":", "erlang/", (list_to_binary(erlang:system_info(version)))/binary, "\r\n",
        "Content-Length", ":", "0", "\r\n",
        "\r\n"
    >>,
    sender:send(State, Response),
    decode_http(State, byte_size(Data), Data, [<<>>]);
handle_http_request(State, Http = #http{fields = Fields}, Body, Data) ->
    Connection = listing:key_get(<<"connection">>, 1, Fields, <<>>),
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Connection>> of
        <<"upgrade">> ->
            handle_http_upgrade(State, Http);
        _ ->
            %% handle http data
            case master:treat(State, Http, Body) of
                {ok, NewState} ->
                    %% next packet
                    decode_http(NewState, byte_size(Data), Data, [<<>>]);
                {stop, Reason, NewState} ->
                    close(NewState#client.socket_type, NewState#client.socket, NewState#client.protocol_type),
                    exit(Reason)
            end
    end.

handle_http_upgrade(State, Http = #http{version = Version, fields = Fields}) ->
    Upgrade = listing:key_get(<<"upgrade">>, 1, Fields, <<>>),
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Upgrade>> of
        <<"websocket">> ->
            handle_web_socket_upgrade(State, Http);
        _ ->
            Response = <<
                Version/binary, " ", "412", " ", "Unsupported Upgrade Method", "\r\n",
                "Connection", ":", "close", "\r\n",
                "\r\n"
            >>,
            sender:send(State, Response)
    end.

handle_web_socket_upgrade(State, Http = #http{fields = Fields}) ->
    Version = listing:key_get(<<"sec-websocket-version">>, 1, Fields, <<>>),
    case Version of
        <<"13">> ->
            %% http upgrade (WebSocket)
            web_socket_handshake(State, Http),
            %% enter web socket stream loop
            Stream = receive_data(State, 0),
            web_socket_header(State#client{protocol_type = web_socket}, Stream, <<>>);
        _ ->
            Response = <<
                Version/binary, " ", "412", " ", "Unsupported WebSocket Version", "\r\n",
                "Connection", ":", "close", "\r\n",
                "\r\n"
            >>,
            sender:send(State, Response)
    end.

%%%===================================================================
%%% http upgrade
%%%===================================================================
%% web socket handshake
web_socket_handshake(State, #http{version = Version, fields = Fields}) ->
    SecKey = listing:key_get(<<"sec-websocket-key">>, 1, Fields, <<>>),
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Encode = base64:encode(Hash),
    Binary = <<
        Version/binary, " ", "101", " ", "Switching Protocols", "\r\n",
        "Upgrade", ":", "WebSocket", "\r\n",
        "Connection", ":", "Upgrade", "\r\n",
        "Sec-WebSocket-Accept", ":", Encode/binary, "\r\n",
        "\r\n"
    >>,
    sender:send(State, Binary).

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
web_socket_header(State, <<_Fin:1, _Rsv:3, 9:4, Mask:1, Length:7, _:Mask/binary-unit:32, Data:Length/binary, Rest/binary>>, Packet) ->
    %% ping packet
    case Length >= 126 of
        true ->
            ?PRINT("Ping Control Frame Payload Size Error: ~p", [Length]),
            exit(normal);
        false ->
            %% send pong packet
            sender:send_pong(State, Data),
            web_socket_header(State, Rest, Packet)
    end;
web_socket_header(State, <<_Fin:1, _Rsv:3, 10:4, Mask:1, Length:7, _:Mask/binary-unit:32, _:Length/binary, Rest/binary>>, Packet) ->
    %% pong
    case Length >= 126 of
        true ->
            ?PRINT("Pong Control Frame Payload Size Error: ~p", [Length]),
            exit(normal);
        false ->
            %% ignore packet
            web_socket_header(State, Rest, Packet)
    end;
web_socket_header(State, <<_Fin:1, _Rsv:3, _:4, _/binary>>, _Packet) ->
    %% close/unknown
    gen_server:cast(State#client.role_pid, {disconnect, normal}),
    close(State#client.socket_type, State#client.socket, State#client.protocol_type),
    exit(normal);
web_socket_header(State, Stream, Packet) ->
    %% incomplete header
    Data = receive_data(State, 0),
    web_socket_header(State, <<Stream/binary, Data/binary>>, Packet).

%% handle web socket body length
web_socket_length(State, <<Mask:1, Length:7, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) when Length =< 125 ->
    web_socket_body(State, Length, Masking, Rest, Packet);
web_socket_length(State, <<Mask:1, 126:7, Length:16, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    web_socket_body(State, Length, Masking, Rest, Packet);
web_socket_length(State, <<Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    web_socket_body(State, Length, Masking, Rest, Packet);
web_socket_length(State, Stream, Packet) ->
    %% incomplete header length
    Data = receive_data(State, 0),
    web_socket_length(State, <<Stream/binary, Data/binary>>, Packet).

%% web socket body unmask
web_socket_body(State, Length, Masking, Rest, Packet) ->
    case Rest of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            web_socket_unmask(State, 0, Masking, Body, RestStream, Packet);
        _ ->
            %% incomplete packet
            web_socket_unmask(State, Length - byte_size(Rest), Masking, Rest, <<>>, Packet)
    end.

%% unmask body
web_socket_unmask(State, Length, Masking = <<Mask:32>>, <<Payload:32, Rest/binary>>, Stream, Acc) ->
    web_socket_unmask(State, Length, Masking, Rest, Stream, <<Acc/binary, (Payload bxor Mask):32>>);
web_socket_unmask(State, Length, <<Mask:24, Rest:8>>, <<Payload:24>>, Stream, Acc) ->
    web_socket_loop(State, Length, <<Rest:8, Mask:24>>, Stream, <<Acc/binary, (Payload bxor Mask):24>>);
web_socket_unmask(State, Length, <<Mask:16, Rest:16>>, <<Payload:16>>, Stream, Acc) ->
    web_socket_loop(State, Length, <<Rest:16, Mask:16>>, Stream, <<Acc/binary, (Payload bxor Mask):16>>);
web_socket_unmask(State, Length, <<Mask:8, Rest:24>>, <<Payload:8>>, Stream, Acc) ->
    web_socket_loop(State, Length, <<Rest:24, Mask:8>>, Stream, <<Acc/binary, (Payload bxor Mask):8>>);
web_socket_unmask(State, Length, Masking, <<>>, Stream, Acc) ->
    %% masking finished
    web_socket_loop(State, Length, Masking, Stream, Acc);
web_socket_unmask(State, Length, <<>>, Body, Stream, Acc) ->
    %% without masking
    web_socket_loop(State, Length, <<>>, Stream, <<Acc/binary, Body/binary>>).

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
                    close(NewState#client.socket_type, NewState#client.socket, State#client.protocol_type),
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
    web_socket_body(State, Length, Masking, <<Stream/binary, Data/binary>>, Packet).

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
