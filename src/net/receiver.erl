%%%-------------------------------------------------------------------
%%% @doc
%%% module receiver
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
-export([handle_web_socket_packet/2, handle_web_socket_packet_old/2, dispatch_web_socket_packet/2]).
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
init([SocketType = gen_tcp, Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = prim_inet:peername(Socket),
    {ok, #client{socket_type = SocketType, socket = Socket, ip = IP}};
init([SocketType = ssl, Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, _Port}} = ssl:peername(Socket),
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
handle_info({inet_async, Socket, Ref, {ok, Data}}, State = #client{socket = Socket, reference = Ref, handler = Handler}) ->
    %% gen tcp
    ?MODULE:Handler(Data, State);
handle_info({Ref, {ok, Data}}, State = #client{reference = Ref, handler = Handler}) ->
    %% ssl
    ?MODULE:Handler(Data, State);
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
    dispatch(<<>>, ?PACKET_HEADER_LENGTH, State#client{handler = handle_tcp_header, protocol = Protocol});
handle_tcp_header(<<Length:16, Protocol:16>>, State) ->
    async_receive(Length, State#client{handler = handle_tcp_body, protocol = Protocol}).

%% tcp packet body
-spec handle_tcp_body(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_tcp_body(Data, State) ->
    dispatch(Data, ?PACKET_HEADER_LENGTH, State#client{handler = handle_tcp_header}).

%% http request
-spec handle_http_request(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_http_request(Data, State = #client{packet = Packet}) ->
    case http:parse_content(<<Packet/binary, Data/binary>>) of
        #http{method = <<"HEAD">>, version = Version} ->
            Response = [
                <<"HTTP/">>, Version, <<" 200 OK\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Date: ">>, list_to_binary(httpd_util:rfc1123_date()), <<"\r\n">>,
                <<"Server: erlang/">>, list_to_binary(erlang:system_info(version)), <<"\r\n">>,
                <<"\r\n">>
            ],
            sender:send(State, list_to_binary(Response)),
            {stop, normal, State#client{packet = <<>>}};
        Http ->
            case binary:matches(http:get_header_field(<<"Upgrade">>, Http), [<<"websocket">>, <<"WebSocket">>, <<"WEBSOCKET">>]) of
                [] ->
                    %% http request
                    master:treat(State#client{packet = <<>>}, Http);
                [_ | _] ->
                    %% http upgrade
                    handle_upgrade(Http, State#client{packet = <<>>})
            end
    end.

%% handle web socket packet (Draft-HiXie-76)
-spec handle_web_socket_packet(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet(<<_:4, 8:4, _:8>>, State) ->
    %% quick close/client close active
    {stop, {shutdown, closed}, State};
handle_web_socket_packet(<<_:8, Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Rest:Length/binary>>, State) ->
    Payload = unmask(Rest, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State);
handle_web_socket_packet(<<_:8, Mask:1, 126:7, Length:64, Masking:Mask/binary-unit:32, Rest:Length/binary>>, State) ->
    Payload = unmask(Rest, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State);
handle_web_socket_packet(<<_:8, Mask:1, Length:7, Masking:Mask/binary-unit:32, Rest:Length/binary>>, State) ->
    Payload = unmask(Rest, Masking, <<>>),
    dispatch_web_socket_packet(Payload, State);
handle_web_socket_packet(Binary, State) ->
    {stop, {web_socket_header_error, Binary}, State}.

%% web handle web socket packet (Draft-HyBi-00)
-spec handle_web_socket_packet_old(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_packet_old(Data, State) ->
    Payload = decode_frames(Data, <<>>),
    dispatch_web_socket_packet(Payload, State).

%%%===================================================================
%%% http upgrade
%%%===================================================================
handle_upgrade(Http, State) ->
    case http:get_header_field(<<"Sec-WebSocket-Key">>, Http) of
        <<>> ->
            SecKey1 = http:get_header_field(<<"Sec-WebSocket-Key1">>, Http),
            SecKey2 = http:get_header_field(<<"Sec-WebSocket-Key2">>, Http),
            handshake(Http, SecKey1, SecKey2, State);
        SecKey ->
            handshake(Http, SecKey, State)
    end.

handshake(Http, SecKey1, SecKey2, State = #client{socket_type = SocketType}) ->
    {SocketType, Scheme} = lists:keyfind(SocketType, 1, [{gen_tcp, <<"ws://">>}, {ssl, <<"wss://">>}]),
    Uri = http:get_uri(Http),
    Host = http:get_header_field(<<"Host">>, Http),
    Origin = http:get_header_field(<<"Origin">>, Http),
    Upgrade = http:get_header_field(<<"Upgrade">>, Http),
    Body = http:get_body(Http),
    Integer1 = erlang:binary_to_integer(<< <<D:8>> || <<D:8>> <= SecKey1, $0 =< D, D =< $9 >>),
    Integer2 = erlang:binary_to_integer(<< <<D:8>> || <<D:8>> <= SecKey2, $0 =< D, D =< $9 >>),
    Blank1 = erlang:byte_size(<< <<S:8>> || <<S:8>> <= SecKey1, S =:= $  >>),
    Blank2 = erlang:byte_size(<< <<S:8>> || <<S:8>> <= SecKey2, S =:= $  >>),
    %% handshake response
    Handshake = [
        <<"HTTP/1.1 101 WebSocket Protocol Handshake\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Origin: ">>, Origin, <<"\r\n">>,
        <<"Sec-WebSocket-Location: ">>, Scheme, Host, Uri, <<"\r\n\r\n">>,
        erlang:md5(<<(Integer1 div Blank1):4/big-unsigned-integer-unit:8, (Integer2 div Blank2):4/big-unsigned-integer-unit:8, Body/binary>>)
    ],
    sender:send(State, list_to_binary(Handshake)),
    async_receive(0, State#client{handler = handle_web_socket_packet_old, protocol_type = 'HyBi'}).

%% web socket handshake
handshake(Http, SecKey, State) ->
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Upgrade = http:get_header_field(<<"Upgrade">>, Http),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)),
    async_receive(0, State#client{handler = handle_web_socket_packet, protocol_type = 'HiXie'}).

%%%===================================================================
%%% web socket decode
%%%===================================================================
%% decode frames (Draft-HyBi-00)
decode_frames(<<>>, Acc) ->
    Acc;
decode_frames(<<0, T/binary>>, Acc) ->
    {Frame, Rest} = parse_frame(T, <<>>),
    decode_frames(Rest, <<Acc/binary, Frame/binary>>).
parse_frame(<<>>, Acc) ->
    {Acc, <<>>};
parse_frame(<<255, Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_frame(<<H, T/binary>>, Acc) ->
    parse_frame(T, <<Acc/binary, H>>).

%% unmask (Draft-HiXie-76)
unmask(<<>>, _Masking, Acc) ->
    Acc;
unmask(PayLoad, <<>>, _) ->
    PayLoad;
unmask(<<A:8>>, <<MA:8, _MB:8, _MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A)>>;
unmask(<<A:8, B:8>>, <<MA:8, MB:8, _MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B)>>;
unmask(<<A:8, B:8, C:8>>, <<MA:8, MB:8, MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
unmask(<<A:8, B:8, C:8, D:8, Rest/binary>>, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    NewAcc = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
    unmask(Rest, Masking, NewAcc).

%%%===================================================================
%%% protocol packet dispatch
%%%===================================================================
%% dispatch protocol packet
dispatch(Binary, Read, State = #client{protocol = Protocol}) ->
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    async_receive(Read, NewState#client{protocol = 0});
                Error ->
                    Error
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [byte_size(Binary), Protocol, Binary]),
            async_receive(Read, State#client{protocol = 0})
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
dispatch_web_socket_packet(_, State) ->
    %% not a complete packet, discard
    async_receive(0, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive data
async_receive(Length, State = #client{socket_type = gen_tcp, socket = Socket}) ->
    case prim_inet:async_recv(Socket, Length, ?TCP_TIMEOUT) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
async_receive(Length, State = #client{socket_type = ssl, socket = #sslsocket{pid = [Pid | _]}, reference = Reference}) ->
    erlang:send(Pid, {'$gen_call', {self(), Reference + 1}, {recv, Length, ?TCP_TIMEOUT}}),
    {noreply, State#client{reference = Reference + 1}}.
