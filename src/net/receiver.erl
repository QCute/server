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
-export([handle_http_request/2]).
-export([handle_web_socket_packet/2]).
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
    async_receive(0, State#client{handler = handle_packet_header});
handle_info({inet_async, Socket, Ref, {ok, InData}}, State = #client{socket = Socket, reference = Ref, handler = Handler, data = Data}) ->
    %% gen tcp
    ?MODULE:Handler(<<Data/binary, InData/binary>>, State#client{data = <<>>});
handle_info({Ref, {ok, InData}}, State = #client{reference = Ref, handler = Handler, data = Data}) ->
    %% ssl
    ?MODULE:Handler(<<Data/binary, InData/binary>>, State#client{data = <<>>});
handle_info({inet_async, Socket, Ref, {error, Reason}}, State = #client{socket = Socket, reference = Ref}) ->
    %% tcp timeout/closed
    {stop, {shutdown, Reason}, State};
handle_info({inet_async, _Socket, Ref, Msg}, State = #client{reference = Ref}) ->
    %% ref not match
    {stop, {shutdown, Msg}, State};
handle_info({inet_async, _Socket, _Ref, Msg}, State) ->
    %% ref not match
    {stop, {shutdown, {ref_not_match, Msg}}, State};
handle_info({inet_reply, _, ok}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?PRINT("Unknown Receiver Message:~w~n", [Info]),
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
handle_packet_header(Data = <<"GET", _/binary>>, State) ->
    %% Length:18245 Protocol: 21536
    handle_http_request(Data, State);
handle_packet_header(Data = <<"POST", _/binary>>, State) ->
    %% Length:20559 Protocol: 21332
    handle_http_request(Data, State);
handle_packet_header(Data, State) ->
    dispatch(Data, State#client{handler = dispatch, protocol_type = tcp}).

%% %% dispatch protocol packet
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
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [byte_size(Binary), Protocol, Binary]),
            async_receive(0, State)
    end;
dispatch(Data, State = #client{protocol_type = tcp}) ->
    %% not completed tcp stream type packet, receive continue
    async_receive(0, State#client{data = Data});
dispatch(_, State) ->
    %% not completed web socket frame type packet, discard
    async_receive(0, State).

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
            case byte_size(Data) < 4096 of
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
            case Length < 65536 of
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
        {ok, {http_header, _, Key = 'Content-Length', undefined, Value}, Rest} ->
            parse_http_header_loop(Rest, Http, binary_to_integer(Value), [{Key, Value} | List]);
        {ok, {http_header, _, Key, undefined, Value}, Rest} ->
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
handshake(Http = #http{version = Version, fields = Fields}, State) ->
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
    async_receive(0, State#client{handler = handle_web_socket_packet, protocol_type = Http}).

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
%% maximum packet size 65535(16K), maximum data size 65535-4
-spec handle_web_socket_packet(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet(<<_:4, 8:4, Mask:1, Length:7, _:Mask/binary-unit:32, _:Length/binary, _/binary>>, State) ->
    %% quick close/client close active
    {stop, {shutdown, closed}, State};
handle_web_socket_packet(<<_:4, _:4, Mask:1, Length:7, Masking:Mask/binary-unit:32, Body:Length/binary, Rest/binary>>, State) ->
    Payload = unmask(Body, Masking, <<>>),
    dispatch(Payload, State#client{data = Rest});
handle_web_socket_packet(<<_:4, _:4, Mask:1, 126:7, Length:16, Masking:Mask/binary-unit:32, Body:Length/binary, Rest/binary>>, State) ->
    Payload = unmask(Body, Masking, <<>>),
    dispatch(Payload, State#client{data = Rest});
handle_web_socket_packet(<<_:4, _:4, Mask:1, 127:7, _Length:64, _Masking:Mask/binary-unit:32, _Rest/binary>>, State) ->
    %% drop prevent packet too large
    {stop, {shutdown, packet_too_large}, State};
handle_web_socket_packet(Data, State) ->
    async_receive(0, State#client{data = Data}).

%%%===================================================================
%%% web socket frame decode
%%%===================================================================
%% unmask
unmask(<<Payload:32, Rest/binary>>, Masking = <<Mask:32, _/binary>>, Acc) ->
    unmask(Rest, Masking, <<Acc/binary, (Payload bxor Mask):32>>);
unmask(<<Payload:24>>, <<Mask:24, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):24>>;
unmask(<<Payload:16>>, <<Mask:16, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):16>>;
unmask(<<Payload:8>>, <<Mask:8, _/binary>>, Acc) ->
    <<Acc/binary, (Payload bxor Mask):8>>;
unmask(<<>>, _, Acc) ->
    Acc;
unmask(Payload, <<>>, _) ->
    Payload.

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
