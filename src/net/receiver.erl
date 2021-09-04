%%%-------------------------------------------------------------------
%%% @doc
%%% tcp receiver
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-behaviour(gen_server).
%% API
-export([start/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% receiver functions
-export([handle_packet_header/2]).
-export([handle_http_request/2]).
-export([handle_web_socket_packet/2, handle_web_socket_packet_more/2]).
-export([dispatch/2]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(Socket :: gen_tcp:socket() | ssl:sslsocket()) -> {ok, pid()} | {error, term()}.
start(Socket) ->
    %% do not mirror by the net supervisor
    gen_server:start(?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(Args :: term()) -> {ok, State :: #client{}}.
init([Socket = #sslsocket{}]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = ssl:peername(Socket),
    {ok, #client{socket = Socket, ip = IP}};
init([Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = prim_inet:peername(Socket),
    {ok, #client{socket = Socket, ip = IP}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #client{}) -> {reply, Reply :: term(), NewState :: #client{}}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, term(), NewState :: #client{}}.
handle_cast({send, Binary}, State) ->
    %% send tcp/http(ws) binary
    sender:send(State, Binary),
    {noreply, State};
handle_cast({stop, Binary}, State) ->
    %% stop and send stop reason to client
    sender:send(State, Binary),
    %% stop this receiver
    {stop, normal, State};
handle_cast(start_receive, State) ->
    %% start receive
    async_receive(0, State#client{handler = handle_packet_header});
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_info({inet_async, Socket, Ref, {ok, InData}}, State = #client{socket = Socket, reference = Ref, handler = Handler, data = Data}) ->
    %% gen tcp data
    ?MODULE:Handler(<<Data/binary, InData/binary>>, State#client{data = <<>>});
handle_info({Ref, {ok, InData}}, State = #client{reference = Ref, handler = Handler, data = Data}) ->
    %% ssl data
    ?MODULE:Handler(<<Data/binary, InData/binary>>, State#client{data = <<>>});
handle_info({inet_async, Socket, Ref, {error, Reason}}, State = #client{socket = Socket, reference = Ref}) ->
    %% gen tcp error, timeout/closed/etc...
    {stop, {shutdown, Reason}, State};
handle_info({Ref, {error, Reason}}, State = #client{reference = Ref}) ->
    %% ssl error, timeout/closed/etc...
    {stop, {shutdown, Reason}, State};
handle_info({inet_reply, Socket, ok}, State = #client{socket = Socket}) ->
    %% sender prim_inet:send/3 => erlang:port_command
    {noreply, State};
handle_info({sender, ok}, State) ->
    %% sender ssl:send/2 => $gen_call
    {noreply, State};
handle_info(Info, State) ->
    ?PRINT("Unknown Receiver Message:~w State: ~w~n", [Info, State]),
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #client{}) -> {ok, NewState :: #client{}}.
terminate(Reason, State = #client{socket = Socket, role_pid = RolePid}) ->
    %% report error if stop abnormal
    Reason =/= normal andalso gen_server:cast(RolePid, {disconnect, Reason}),
    %% close socket
    _ = (is_record(Socket, sslsocket) andalso ssl:close(Socket) == ok) orelse gen_tcp:close(Socket) == ok,
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #client{}, Extra :: term()) -> {ok, NewState :: #client{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Receiver functions
%%%===================================================================
%% handle tcp/http packet
-spec handle_packet_header(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_packet_header(Data = <<"GET", _/binary>>, State) ->
    %% Length:18245 Protocol: 21536
    handle_http_request(Data, State);
handle_packet_header(Data = <<"POST", _/binary>>, State) ->
    %% Length:20559 Protocol: 21332
    handle_http_request(Data, State);
handle_packet_header(Data, State) ->
    dispatch(Data, State#client{handler = dispatch, protocol_type = tcp}).

%% http request
-spec handle_http_request(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_http_request(Data, State) ->
    case parse_http_request(Data) of
        {Http = #http{fields = Fields}, Rest} ->
            case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= proplists:get_value('Upgrade', Fields, <<"">>)>> of
                <<"websocket">> ->
                    %% http upgrade (WebSocket)
                    handshake(Http, State#client{data = Rest});
                _ ->
                    %% normal http request
                    master:treat(State#client{data = Rest}, Http)
            end;
        {more, _} ->
            case byte_size(Data) =< 4096 of
                true ->
                    %% maximum header size 4096
                    async_receive(0, State#client{handler = handle_http_request, data = Data});
                false ->
                    Result = json:encode([{result, <<"header size exceeded limited size">>}]),
                    Response = [
                        <<"HTTP/1.1">>, <<" 200 OK\r\n">>,
                        <<"Connection: close\r\n">>,
                        <<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>,
                        <<"Content-Type: application/json">>, <<"\r\n">>,
                        <<"Content-Length: ">>, integer_to_binary(byte_size(Result)), <<"\r\n">>,
                        <<"\r\n">>, Result
                    ],
                    sender:send(State, list_to_binary(Response)),
                    %% not complete
                    {stop, normal, State}
            end;
        {body, Length} ->
            case Length =< 65536 of
                true ->
                    %% maximum body size 65536
                    async_receive(Length, State#client{handler = handle_http_request, data = Data});
                false ->
                    Result = json:encode([{result, <<"body size exceeded limited size">>}]),
                    Response = [
                        <<"HTTP/1.1">>, <<" 200 OK\r\n">>,
                        <<"Connection: close\r\n">>,
                        <<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>,
                        <<"Content-Type: application/json">>, <<"\r\n">>,
                        <<"Content-Length: ">>, integer_to_binary(byte_size(Result)), <<"\r\n">>,
                        <<"\r\n">>, Result
                    ],
                    sender:send(State, list_to_binary(Response)),
                    %% not complete
                    {stop, normal, State}
            end;
        Error ->
            %% unknown error
            {stop, {shutdown, Error}, State}
    end.

%%%===================================================================
%%% http request parse
%%%===================================================================
parse_http_request(Data) ->
    case erlang:decode_packet(http_bin, Data, []) of
        {ok, {http_request, Method, {abs_path, Uri}, {Version, Revision}}, Rest} ->
            Http = #http{method = Method, uri = Uri, version = <<"HTTP/", (integer_to_binary(Version))/binary, ".", (integer_to_binary(Revision))/binary>>},
            parse_http_header_loop(Rest, Http, 0, []);
        Error ->
            Error
    end.

parse_http_header_loop(Data, Http, ContentLength, List) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, {http_header, _, Key = 'Content-Length', _, Value}, Rest} ->
            parse_http_header_loop(Rest, Http, binary_to_integer(Value), [{Key, Value} | List]);
        {ok, {http_header, _, Key, _, Value}, Rest} ->
            parse_http_header_loop(Rest, Http, ContentLength, [{Key, Value} | List]);
        {ok, http_eoh, <<Body:ContentLength/binary, Rest/binary>>} ->
            {Http#http{fields = parse_web_socket_header(List, []), body = Body}, Rest};
        {ok, http_eoh, Body} ->
            {body, ContentLength - byte_size(Body)};
        Error ->
            Error
    end.

parse_web_socket_header([], List) ->
    List;
parse_web_socket_header([{Key, Value} | T], List) when is_binary(Key) ->
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Key>> of
        <<"sec-websocket-key">> ->
            parse_web_socket_header(T, [{'Sec-WebSocket-Key', Value} | List]);
        <<"sec-websocket-version">> ->
            parse_web_socket_header(T, [{'Sec-WebSocket-Version', Value} | List]);
        <<"sec-websocket-extensions">> ->
            parse_web_socket_header(T, [{'Sec-WebSocket-Extensions', Value} | List]);
        _ ->
            parse_web_socket_header(T, [{Key, Value} | List])
    end;
parse_web_socket_header([H | T], List) ->
    parse_web_socket_header(T, [H | List]).

%%%===================================================================
%%% http upgrade
%%%===================================================================
%% web socket handshake
handshake(#http{version = Version, fields = Fields}, State) ->
    Upgrade = proplists:get_value('Upgrade', Fields, <<"">>),
    SecKey = proplists:get_value('Sec-WebSocket-Key', Fields, <<"">>),
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        Version, <<" 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)),
    async_receive(0, State#client{handler = handle_web_socket_packet, protocol_type = web_socket}).

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

%% handle web socket packet
-spec handle_web_socket_packet(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet(<<_:4, 8:4, Mask:1, Length:7, _:Mask/binary-unit:32, _:Length/binary, _/binary>>, State) ->
    %% close frame
    {stop, {shutdown, closed}, State};
handle_web_socket_packet(<<_:4, _:4, Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Rest/binary>>, State) ->
    %% 64 bit body length
    handle_web_socket_packet_more(Rest, State#client{length = Length, masking = Masking});
handle_web_socket_packet(<<_:4, _:4, Mask:1, 126:7, Length:16, Masking:Mask/binary-unit:32, Rest/binary>>, State) ->
    %% 16 bit body length
    handle_web_socket_packet_more(Rest, State#client{length = Length, masking = Masking});
handle_web_socket_packet(<<_:4, _:4, Mask:1, Length:7, Masking:Mask/binary-unit:32, Rest/binary>>, State) ->
    %% 8 bit body length
    handle_web_socket_packet_more(Rest, State#client{length = Length, masking = Masking});
handle_web_socket_packet(Data, State) ->
    %% continue
    async_receive(0, State#client{data = Data}).

%% handle web socket packet more
-spec handle_web_socket_packet_more(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet_more(Data, State = #client{length = Length, masking = Masking, body = RestBody}) ->
    case Data of
        <<Body:Length/binary, Rest/binary>> ->
            {Payload, _} = unmask(Body, Masking, <<>>),
            dispatch(Payload, State#client{handler = handle_web_socket_packet, data = Rest});
        _ ->
            {Payload, Masked} = unmask(Data, Masking, <<>>),
            dispatch(<<RestBody/binary, Payload/binary>>, State#client{handler = handle_web_socket_packet_more, length = Length - byte_size(Data), masking = Masked})
    end.

%%%===================================================================
%%% web socket frame decode
%%%===================================================================
%% unmask
unmask(<<Payload:32, Rest/binary>>, Masking = <<Mask:32>>, Acc) ->
    unmask(Rest, Masking, <<Acc/binary, (Payload bxor Mask):32>>);
unmask(<<Payload:24>>, <<Mask:24, Rest:8>>, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):24>>, <<Rest:8, Mask:24>>};
unmask(<<Payload:16>>, <<Mask:16, Rest:16>>, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):16>>, <<Rest:16, Mask:16>>};
unmask(<<Payload:8>>, <<Mask:8, Rest:24>>, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):8>>, <<Rest:24, Mask:8>>};
unmask(<<>>, Mask, Acc) ->
    {Acc, Mask};
unmask(Payload, <<>>, _) ->
    {Payload, <<>>}.

%%%===================================================================
%%% dispatch protocol data
%%%===================================================================
%% dispatch 
-spec dispatch(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
dispatch(<<Length:16, Protocol:16, Binary:Length/binary, Rest/binary>>, State) ->
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    dispatch(Rest, NewState);
                Error ->
                    Error
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [Length, Protocol, Binary]),
            dispatch(Rest, State)
    end;
dispatch(Data, State = #client{protocol_type = tcp}) ->
    %% not completed tcp stream type packet, receive continue
    async_receive(0, State#client{data = Data});
dispatch(Data, State) ->
    async_receive(0, State#client{body = Data}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% async receive data
async_receive(Length, State = #client{socket = #sslsocket{pid = [Pid | _]}, reference = Reference}) ->
    erlang:send(Pid, {'$gen_call', {self(), Reference + 1}, {recv, Length, ?MINUTE_MILLISECONDS}}),
    {noreply, State#client{reference = Reference + 1}};
async_receive(Length, State = #client{socket = Socket}) ->
    case prim_inet:async_recv(Socket, Length, ?MINUTE_MILLISECONDS) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end.
