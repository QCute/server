%%%-------------------------------------------------------------------
%%% @doc
%%% tcp receiver
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-behaviour(gen_server).
%% API
-export([start/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% receiver functions
-export([handle_packet_header/2]).
-export([handle_tcp_header/2, handle_tcp_body/2]).
-export([handle_http_request/2]).
-export([handle_web_socket_packet/2]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include("common.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> {ok, pid()} | {error, term()}.
start(SocketType, Socket) ->
    %% do not mirror by the net supervisor
    gen_server:start(?MODULE, [SocketType, Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(Args :: term()) -> {ok, State :: #client{}}.
init([SocketType, Socket = #sslsocket{}]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = ssl:peername(Socket),
    {ok, #client{socket_type = SocketType, socket = Socket, ip = IP}};
init([SocketType, Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = prim_inet:peername(Socket),
    {ok, #client{socket_type = SocketType, socket = Socket, ip = IP}}.

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
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_info(start_receive, State) ->
    %% start receive
    async_receive(?PACKET_HEADER_LENGTH, State#client{handler = handle_packet_header});
handle_info({inet_async, Socket, Ref, {ok, Data}}, State = #client{socket = Socket, reference = Ref, handler = Handler, packet = Packet}) ->
    %% gen tcp
    ?MODULE:Handler(<<Packet/binary, Data/binary>>, State#client{packet = <<>>});
handle_info({Ref, {ok, Data}}, State = #client{reference = Ref, handler = Handler, packet = Packet}) ->
    %% ssl
    ?MODULE:Handler(<<Packet/binary, Data/binary>>, State#client{packet = <<>>});
handle_info({inet_async, Socket, Ref, {error, Reason}}, State = #client{socket = Socket, reference = Ref}) ->
    %% tcp timeout/closed
    {stop, {shutdown, Reason}, State};
handle_info({inet_async, _Socket, Ref, Msg}, State = #client{reference = Ref}) ->
    %% ref not match
    {stop, {shutdown, Msg}, State};
handle_info({inet_async, _Socket, _Ref, Msg}, State) ->
    %% ref not match
    {stop, {shutdown, {ref_not_match, Msg}}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #client{}) -> {ok, NewState :: #client{}}.
terminate(Reason, State = #client{socket_type = SocketType, socket = Socket, role_pid = RolePid}) ->
    %% report error if stop abnormal
    Reason =/= normal andalso gen_server:cast(RolePid, {disconnect, Reason}),
    %% close socket
    catch SocketType:close(Socket),
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
handle_packet_header(Data = <<"GET ">>, State) ->
    async_receive(0, State#client{handler = handle_http_request, packet = Data});
handle_packet_header(Data = <<"POST">>, State) ->
    async_receive(0, State#client{handler = handle_http_request, packet = Data});
handle_packet_header(Data = <<"HEAD">>, State) ->
    async_receive(0, State#client{handler = handle_http_request, packet = Data});
handle_packet_header(Data, State) ->
    handle_tcp_header(Data, State#client{protocol_type = tcp}).

%% tcp packet header
-spec handle_tcp_header(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_tcp_header(<<0:16, Protocol:16>>, State) ->
    dispatch(<<>>, State#client{handler = handle_tcp_header, protocol = Protocol});
handle_tcp_header(<<Length:16, Protocol:16>>, State) ->
    async_receive(Length, State#client{handler = handle_tcp_body, protocol = Protocol}).

%% tcp packet body
-spec handle_tcp_body(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_tcp_body(Data, State) ->
    dispatch(Data, State#client{handler = handle_tcp_header}).

%% http request
-spec handle_http_request(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_http_request(Data, State) ->
    case httpd_request:parse([Data, []]) of
        {ok, {"HEAD", _, Version, {_, _}, <<>>}} ->
            Response = [
                Version, <<" 200 OK\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Date: ">>, list_to_binary(httpd_util:rfc1123_date()), <<"\r\n">>,
                <<"Server: erlang/">>, list_to_binary(erlang:system_info(version)), <<"\r\n">>,
                <<"\r\n">>
            ],
            sender:send(State, list_to_binary(Response)),
            {stop, normal, State};
        {ok, {Method, Uri, Version, {_, Fields}, Body}} ->
            case string:to_lower(proplists:get_value("upgrade", Fields, "")) of
                "websocket" ->
                    %% http upgrade
                    handshake(Fields, State);
                _ ->
                    %% normal http request
                    master:treat(State, #http{method = Method, uri = Uri, version = Version, fields = Fields, body = Body})
            end;
        _ ->
            %% not complete
            async_receive(0, State#client{handler = handle_http_request, packet = Data})
    end.

%% handle web socket packet
-spec handle_web_socket_packet(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet(<<_:4, 8:4, Mask:1, Length:7, _:Mask/binary-unit:32, _:Length/binary, _/binary>>, State) ->
    %% quick close/client close active
    {stop, {shutdown, closed}, State};
handle_web_socket_packet(<<_:8, Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Body:Length/binary, Rest/binary>>, State) ->
    %% @todo drop prevent packet too large
    %% {stop, {shutdown, packet_too_large}, State};
    Payload = unmask(Body, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State#client{packet = Rest});
handle_web_socket_packet(<<_:8, Mask:1, 126:7, Length:64, Masking:Mask/binary-unit:32, Body:Length/binary, Rest/binary>>, State) ->
    Payload = unmask(Body, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State#client{packet = Rest});
handle_web_socket_packet(<<_:8, Mask:1, Length:7, Masking:Mask/binary-unit:32, Body:Length/binary, Rest/binary>>, State) ->
    Payload = unmask(Body, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State#client{packet = Rest});
handle_web_socket_packet(Data = <<_:8, Mask:1, 127:7, Length:64, _:Mask/binary-unit:32, Rest/binary>>, State) ->
    %% @todo drop prevent packet too large
    %% {stop, {shutdown, packet_too_large}, State};
    async_receive(Length - byte_size(Rest), State#client{packet = Data});
handle_web_socket_packet(Data = <<_:8, Mask:1, 126:7, Length:64, _:Mask/binary-unit:32, Rest/binary>>, State) ->
    async_receive(Length - byte_size(Rest), State#client{packet = Data});
handle_web_socket_packet(Data = <<_:8, Mask:1, Length:7, _:Mask/binary-unit:32, Rest/binary>>, State) ->
    async_receive(Length - byte_size(Rest), State#client{packet = Data});
handle_web_socket_packet(Data, State) ->
    async_receive(0, State#client{packet = Data}).

%% Web Socket Draft-HiXie-76 Packet Protocol
%% Header
%% +---------+---------------------------------+
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

%%%===================================================================
%%% http upgrade
%%%===================================================================
%% web socket handshake
handshake(Fields, State) ->
    Upgrade = proplists:get_value("upgrade", Fields, ""),
    SecKey = proplists:get_value("sec-websocket-key", Fields, ""),
    Hash = crypto:hash(sha, [SecKey, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"]),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)),
    async_receive(0, State#client{handler = handle_web_socket_packet, protocol_type = web_socket}).

%%%===================================================================
%%% web socket decode
%%%===================================================================
%% unmask
unmask(<<>>, _Masking, Acc) ->
    Acc;
unmask(PayLoad, <<>>, _) ->
    PayLoad;
unmask(<<Payload:8>>, <<Mask:8, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):8>>;
unmask(<<Payload:16>>, <<Mask:16, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):16>>;
unmask(<<Payload:24>>, <<Mask:24, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):24>>;
unmask(<<Payload:32, Rest/binary>>, Masking = <<Mask:32, _/binary>>, Acc) ->
    unmask(Rest, Masking, <<Acc/binary, (Payload bxor Mask):32>>).

%%%===================================================================
%%% protocol packet dispatch
%%%===================================================================
%% dispatch protocol packet
dispatch(Binary, State = #client{protocol = Protocol}) ->
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    async_receive(?PACKET_HEADER_LENGTH, NewState#client{protocol = 0});
                Error ->
                    Error
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [byte_size(Binary), Protocol, Binary]),
            async_receive(?PACKET_HEADER_LENGTH, State#client{protocol = 0})
    end.

%% dispatch web socket protocol packet
dispatch_web_socket_packet(<<Length:16, Protocol:16, Binary:Length/binary, Rest/binary>>, State)  ->
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State#client{protocol = Protocol}, Data) of
                {ok, NewState} ->
                    dispatch_web_socket_packet(Rest, NewState#client{protocol = 0});
                Error ->
                    Error
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [byte_size(Binary), Protocol, Binary]),
            dispatch_web_socket_packet(Rest, State)
    end;
dispatch_web_socket_packet(_, State = #client{packet = <<>>}) ->
    %% not a complete packet, discard
    async_receive(0, State);
dispatch_web_socket_packet(_, State = #client{handler = Handler, packet = Packet}) ->
    %% not a complete packet, discard
    ?MODULE:Handler(Packet, State#client{packet = <<>>}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive data
async_receive(Length, State = #client{socket = #sslsocket{pid = [Pid | _]}, reference = Reference}) ->
    erlang:send(Pid, {'$gen_call', {self(), Reference + 1}, {recv, Length, ?TCP_TIMEOUT}}),
    {noreply, State#client{reference = Reference + 1}};
async_receive(Length, State = #client{socket = Socket}) ->
    case prim_inet:async_recv(Socket, Length, ?TCP_TIMEOUT) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end.
