%%%-------------------------------------------------------------------
%%% @doc
%%% module websocket reader
%%% @end
%%%-------------------------------------------------------------------
-module(web_socket).
%% API
-export([
    handle_http_head/2,
    handle_html5_head/2,
    handle_html5_body_length/2,
    decode/2
]).
%% Includes
-include("socket.hrl").
%% Records
-record(http_head, {method, path, version, headers}).

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 区分h5
handle_http_head(Data, State) ->
    HttpHead = parse_http_head(type:to_list(Data)),
    Value = get_header_value("Upgrade", HttpHead),
    %% 确认websocket
    case Value =/= [] andalso string:to_lower(Value) =:= "websocket" of
        true ->
            SecKey = get_header_value("Sec-WebSocket-Key", HttpHead),
            SecKey1 = get_header_value("Sec-WebSocket-Key1", HttpHead),
            SecKey2 = get_header_value("Sec-WebSocket-Key2", HttpHead),
            case SecKey =/= [] of
                true ->
                    %% ws
                    hand_shake(State, SecKey);
                false when SecKey1 =/= [] andalso SecKey2 =/= [] ->
                    %% wss
                    hand_shake(State, HttpHead, SecKey1, SecKey2);
                _ ->
                    {stop, {no_ws_handshake, HttpHead}, State}
            end;
        _ ->
            {stop, {not_websocket, HttpHead}, State}
    end.

%% WebSocket OpCode 定义
%% 0 表示连续消息片断
%% 1 表示文本消息片断
%% 2 表未二进制消息片断
%% 3-7 为将来的非控制消息片断保留的操作码
%% 8 表示连接关闭
%% 9 表示心跳检查的ping
%% A 表示心跳检查的pong
%% B-F 为将来的控制消息片断的保留操作码

%% h5 协议头
handle_html5_head(<<_Fin:1, _Rsv:3, 8:4, _Msk:1, _Length:7>>, State) ->
    %% quick close/ client close active
    {stop, closed, State};
handle_html5_head(<<_Fin:1, _Rsv:3, _OpCode:4, _Mask:1, 127:7>>, State) ->
    {read, 12, ?HEART_TIMEOUT, State#client{state = wait_html5_body_length, h5_length = 127}};
handle_html5_head(<<_Fin:1, _Rsv:3, _OpCode:4, _Mask:1, 126:7>>, State) ->
    {read, 6, ?HEART_TIMEOUT, State#client{state = wait_html5_body_length, h5_length = 126}};
handle_html5_head(<<_Fin:1, _Rsv:3, _OpCode:4, _Mask:1, Length:7>>, State) ->
    {read, 4, ?HEART_TIMEOUT, State#client{state = wait_html5_body_length, h5_length = Length}};
handle_html5_head(Binary, State) ->
    {stop, {h5_head_error, Binary}, State}.

%% h5 掩码，长度读取（安全验证）
handle_html5_body_length(<<BodyLength:64, Masking:4/binary>>, State = #client{h5_length = 127}) when BodyLength >= 4 ->
    {read, BodyLength, ?TCP_TIMEOUT, State#client{state = wait_html5_body, h5_length = BodyLength, masking_h5 = Masking}};
handle_html5_body_length(<<BodyLength:16, Masking:4/binary>>, State = #client{h5_length = 126}) when BodyLength >= 4 ->
    {read, BodyLength, ?TCP_TIMEOUT, State#client{state = wait_html5_body, h5_length = BodyLength, masking_h5 = Masking}};
handle_html5_body_length(<<Masking:4/binary>>, State = #client{h5_length = BodyLength}) when BodyLength >= 4 ->
    %% length 16 bit and protocol 16 bit
    {read, BodyLength, ?TCP_TIMEOUT, State#client{state = wait_html5_body, masking_h5 = Masking}};
handle_html5_body_length(<<Masking:4/binary>>, State = #client{h5_length = BodyLength}) ->
    %% continue length
    {read, BodyLength, ?TCP_TIMEOUT, State#client{state = wait_html5_body, masking_h5 = Masking}};
handle_html5_body_length(Binary, State) ->
    {stop, {h5_head_length_error, Binary}, State}.

%% @doc WebSocket解码
decode(#client{connect_type = hy_bi, masking_h5 = Masking}, Data) ->
    {unmask(Data, Masking), 2, wait_html5_head};
decode(#client{connect_type = hi_xie}, Data) ->
    {frames(Data, []), 0, wait_html5_body}.

%% @doc 掩码计算
unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).
unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size(Payload) of
        0 ->
            Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _ ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.

%% @doc 帧计算
frames(<<>>, Frames) ->
    lists:reverse(Frames);
frames(<<0, T/binary>>, Frames) ->
    {Frame, Rest} = frame_parse(T, <<>>),
    frames(Rest, [Frame | Frames]).
frame_parse(<<255, Rest/binary>>, Buffer) ->
    {Buffer, Rest};
frame_parse(<<H, T/binary>>, Buffer) ->
    frame_parse(T, <<Buffer/binary, H>>).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% websocket 挥手
hand_shake(State, SecKey) ->
    Hash = crypto:hash(sha, SecKey ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:response(State, Binary),
    {read, 2, ?TCP_TIMEOUT, State#client{state = wait_html5_head, connect_type = hy_bi}}.
hand_shake(State = #client{socket_type = SocketType}, HttpHead, SecKey1, SecKey2) ->
    case SocketType of
        ssl ->
            Scheme = "wss://";
        gen_tcp ->
            Scheme = "ws://"
    end,
    Origin = get_header_value("Origin", HttpHead),
    Host = get_header_value("Host", HttpHead),
    Path = HttpHead#http_head.path,
    Body = get_header_value(body, HttpHead),
    Ikey1 = [D || D <- SecKey1, $0 =< D, D =< $9],
    Ikey2 = [D || D <- SecKey2, $0 =< D, D =< $9],
    Blank1 = length([D || D <- SecKey1, D =:= 32]),
    Blank2 = length([D || D <- SecKey2, D =:= 32]),
    Part1 = erlang:list_to_integer(Ikey1) div Blank1,
    Part2 = erlang:list_to_integer(Ikey2) div Blank2,
    BodyBin = list_to_binary(Body),
    CKey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, BodyBin/binary>>,
    Challenge = erlang:md5(CKey),
    Location = lists:concat([Scheme, Host, Path]),
    Handshake = [
        <<"HTTP/1.1 101 WebSocket Protocol Handshake\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Upgrade: WebSocket\r\n">>,
        <<"Sec-WebSocket-Origin: ">>, Origin, <<"\r\n">>,
        <<"Sec-WebSocket-Location: ">>, Location, <<"\r\n\r\n">>,
        Challenge
    ],
    sender:response(State, Handshake),
    {read, 0, ?TCP_TIMEOUT, State#client{state = wait_html5_body, connect_type = hi_xie}}.

%% 获取协议头内容
get_header_value(Key, #http_head{headers = Headers}) ->
    case lists:keyfind(Key, 1, Headers) of
        {Key, Value} ->
            Value;
        false ->
            []
    end.

%% 解析http头
parse_http_head(Data) ->
    {Headers, _, HttpHead} = do_parse_http_head(Data),
    %% GET / HTTP/1.1
    %% POST / HTTP/1.1
    [Method, Path, Version] = string:tokens(HttpHead, " "),
    #http_head{
        method = Method,
        path = Path,
        version = Version,
        headers = Headers
    }.
do_parse_http_head([]) ->
    {[], [], []};
do_parse_http_head([$\r, $\n | T]) ->
    {Headers, Key, Value} = do_parse_http_head(T),
    case Key of
        "" when Headers =:= [], Value =/= "" ->
            {[{body, Value} | Headers], "", ""};
        "" ->
            {Headers, Key, Value};
        _ ->
            {[{Key, Value} | Headers], "", ""}
    end;
do_parse_http_head([$\n, $\n | T]) ->
    {Headers, Key, Value} = do_parse_http_head(T),
    case Key of
        "" ->
            {Headers, Key, Value};
        _ ->
            {[{Key, Value} | Headers], "", ""}
    end;
do_parse_http_head([C, $:, $\  | T]) ->
    {Headers, _Key, Value} = do_parse_http_head(T),
    {Headers, [C], Value};
do_parse_http_head([C | T]) ->
    {Headers, Key, Value} = do_parse_http_head(T),
    case Key of
        "" ->
            {Headers, Key, [C|Value]};
        _ ->
            {Headers, [C|Key], Value}
    end.
