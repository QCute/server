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
-export([handle_http_header/2, handle_http_request/2]).
-export([handle_web_socket_header/2, handle_web_socket_body_length/2, handle_web_socket_body/2, read_web_socket_packet/2]).
-export([handle_web_socket_body_old/2, read_web_socket_packet_old/2]).
%% Includes
-include("common.hrl").
-include("socket.hrl").
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
    async_receive(?PACKET_HEADER_LENGTH, State#client{state = handle_packet_header});
handle_info({inet_async, Socket, Ref, {ok, Data}}, State = #client{socket = Socket, reference = Ref}) ->
    %% main receive & handle tpc data
    ?MODULE:(State#client.state)(Data, State);
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
    async_receive(8, State#client{state = handle_http_header, packet = Data});
handle_packet_header(Data = <<"POST">>, State) ->
    async_receive(8, State#client{state = handle_http_header, packet = Data});
handle_packet_header(Data = <<"HEAD">>, State) ->
    async_receive(8, State#client{state = handle_http_header, packet = Data});
handle_packet_header(Data, State) ->
    handle_tcp_header(Data, State#client{protocol_type = tcp}).

%% tcp packet header
-spec handle_tcp_header(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_tcp_header(<<?PACKET_HEADER_LENGTH:16, Protocol:16>>, State) ->
    dispatch(<<>>, ?PACKET_HEADER_LENGTH, State#client{state = handle_tcp_header, protocol = Protocol});
handle_tcp_header(<<Length:16, Protocol:16>>, State) ->
    async_receive(Length - ?PACKET_HEADER_LENGTH, State#client{state = handle_tcp_body, protocol = Protocol}).

%% tcp packet body
-spec handle_tcp_body(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_tcp_body(Data, State) ->
    dispatch(Data, ?PACKET_HEADER_LENGTH, State#client{state = handle_tcp_header}).

%% http header
-spec handle_http_header(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_http_header(Data = <<"/ HTTP/", _/binary>>, State = #client{packet = Packet}) ->
    async_receive(0, State#client{state = handle_http_request, packet = <<Packet/binary, Data/binary>>});
handle_http_header(Data, State) ->
    {stop, {shutdown, {handle_http_header, Data}}, State}.

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
                    %% web socket upgrade
                    {Read, NewState} = web_socket:handle_upgrade(Http, State#client{packet = <<>>}),
                    async_receive(Read, NewState#client{read_length = ?PACKET_HEADER_LENGTH})
            end
    end.

%% handle web socket header (Draft-HiXie-76)
-spec handle_web_socket_header(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_header(<<_Fin:1, _Rsv:3, 8:4, _Msk:1, _Length:7>>, State) ->
    %% quick close/client close active
    {stop, {shutdown, closed}, State};
handle_web_socket_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, 127:7>>, State) ->
    async_receive(8 + Mask * 4, State#client{state = handle_web_socket_body_length, packet_length = 127});
handle_web_socket_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, 126:7>>, State) ->
    async_receive(2 + Mask * 4, State#client{state = handle_web_socket_body_length, packet_length = 126});
handle_web_socket_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, Length:7>>, State) ->
    async_receive(0 + Mask * 4, State#client{state = handle_web_socket_body_length, packet_length = Length});
handle_web_socket_header(Binary, State) ->
    {stop, {web_socket_header_error, Binary}, State}.

%% handle web socket body length and masking (Draft-HiXie-76)
-spec handle_web_socket_body_length(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_body_length(<<Masking:4/binary>>, State = #client{packet_length = 0}) ->
    %% empty packet
    async_receive(?WEB_SOCKET_HEADER_LENGTH, State#client{state = handle_web_socket_header, packet_length = 0, masking = Masking});
handle_web_socket_body_length(<<BodyLength:64, Masking:4/binary>>, State = #client{packet_length = 127, read_length = ReadLength}) ->
    async_receive(min(BodyLength, ReadLength), State#client{state = handle_web_socket_body, packet_length = BodyLength, masking = Masking});
handle_web_socket_body_length(<<BodyLength:16, Masking:4/binary>>, State = #client{packet_length = 126, read_length = ReadLength}) ->
    async_receive(min(BodyLength, ReadLength), State#client{state = handle_web_socket_body, packet_length = BodyLength, masking = Masking});
handle_web_socket_body_length(<<Masking:4/binary>>, State = #client{packet_length = BodyLength, read_length = ReadLength}) ->
    async_receive(min(BodyLength, ReadLength), State#client{state = handle_web_socket_body, packet_length = BodyLength, masking = Masking});
handle_web_socket_body_length(Binary, State) ->
    {stop, {web_socket_body_length_error, Binary}, State}.

%% web socket data (Draft-HiXie-76)
-spec handle_web_socket_body(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_body(Data, State = #client{packet = Packet, packet_length = PacketLength, read_length = ReadLength}) ->
    {PayLoad, NewState} = web_socket:decode(Data, State),
    BinarySize = byte_size(Packet) + byte_size(PayLoad),
    case ReadLength =< BinarySize of
        true ->
            read_web_socket_packet(<<Packet/binary, PayLoad/binary>>, NewState#client{packet = <<>>, packet_length = PacketLength - byte_size(Data)});
        false ->
            async_receive(?WEB_SOCKET_HEADER_LENGTH, NewState#client{state = handle_web_socket_header, packet = <<Packet/binary, PayLoad/binary>>, read_length = ReadLength - BinarySize})
    end.

%% web socket packet (Draft-HiXie-76)
-spec read_web_socket_packet(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
read_web_socket_packet(<<?PACKET_HEADER_LENGTH:16, Protocol:16>>, State = #client{protocol = 0, packet_length = 0}) ->
    %% complete header
    dispatch(<<>>, ?WEB_SOCKET_HEADER_LENGTH, State#client{state = handle_web_socket_header, protocol = Protocol, read_length = ?PACKET_HEADER_LENGTH});
read_web_socket_packet(<<?PACKET_HEADER_LENGTH:16, Protocol:16>>, State = #client{protocol = 0, packet_length = PacketLength}) ->
    case ?PACKET_HEADER_LENGTH =< PacketLength of
        true ->
            %% complete header
            dispatch(<<>>, ?PACKET_HEADER_LENGTH, State#client{state = handle_web_socket_body, protocol = Protocol, read_length = ?PACKET_HEADER_LENGTH});
        false ->
            %% incomplete header
            dispatch(<<>>, PacketLength, State#client{state = handle_web_socket_body, protocol = Protocol, read_length = ?PACKET_HEADER_LENGTH})
    end;
read_web_socket_packet(<<Length:16, Protocol:16>>, State = #client{protocol = 0, packet_length = 0}) ->
    %% incomplete packet
    async_receive(?WEB_SOCKET_HEADER_LENGTH, State#client{state = handle_web_socket_header, protocol = Protocol, read_length = Length - ?PACKET_HEADER_LENGTH});
read_web_socket_packet(<<Length:16, Protocol:16>>, State = #client{protocol = 0, packet_length = PacketLength}) ->
    case Length - ?PACKET_HEADER_LENGTH =< PacketLength of
        true ->
            %% complete packet
            async_receive(Length - ?PACKET_HEADER_LENGTH, State#client{state = handle_web_socket_body, protocol = Protocol, read_length = Length - ?PACKET_HEADER_LENGTH});
        false ->
            %% incomplete packet
            async_receive(PacketLength, State#client{state = handle_web_socket_body, protocol = Protocol, read_length = Length - ?PACKET_HEADER_LENGTH})
    end;
read_web_socket_packet(Data, State = #client{packet_length = 0}) ->
    %% complete packet
    dispatch(Data, ?WEB_SOCKET_HEADER_LENGTH, State#client{state = handle_web_socket_header, read_length = ?PACKET_HEADER_LENGTH});
read_web_socket_packet(Data, State = #client{packet_length = PacketLength}) ->
    case ?PACKET_HEADER_LENGTH =< PacketLength of
        true ->
            %% complete header
            dispatch(Data, ?PACKET_HEADER_LENGTH, State#client{state = handle_web_socket_body, read_length = ?PACKET_HEADER_LENGTH});
        false ->
            %% incomplete header
            dispatch(Data, PacketLength, State#client{state = handle_web_socket_body, read_length = ?PACKET_HEADER_LENGTH})
    end.

%% web socket data (Draft-HyBi-00)
-spec handle_web_socket_body_old(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
handle_web_socket_body_old(Data, State = #client{packet = Packet}) ->
    PayLoad = web_socket:decode(Data, State),
    read_web_socket_packet_old(<<Packet/binary, PayLoad/binary>>, State#client{packet = <<>>}).

%% web socket packet (Draft-HyBi-00)
-spec read_web_socket_packet_old(Data :: binary(), State :: #client{}) -> {noreply, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
read_web_socket_packet_old(<<Length:16, Protocol:16, Rest/binary>>, State) when Length - ?PACKET_HEADER_LENGTH =< byte_size(Rest) ->
    BinaryLength = Length - ?PACKET_HEADER_LENGTH,
    <<Binary:BinaryLength/binary, NewRest/binary>> = Rest,
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    read_web_socket_packet_old(NewRest, NewState#client{protocol = 0});
                Error ->
                    Error
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [?PACKET_HEADER_LENGTH + byte_size(Binary), Protocol, Binary]),
            read_web_socket_packet_old(NewRest, State#client{protocol = 0})
    end;
read_web_socket_packet_old(Data, State) ->
    %% not complete packet
    async_receive(?FRAME_LENGTH, State#client{state = handle_web_socket_body_old, packet = Data}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive data
async_receive(Length, State = #client{socket = Socket, socket_type = gen_tcp}) ->
    case prim_inet:async_recv(Socket, Length, ?TCP_TIMEOUT) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
async_receive(Length, State = #client{socket = Socket, socket_type = ssl}) ->
    Self = self(),
    Pid = spawn(fun() -> erlang:send(Self, {inet_async, Socket, self(), catch ssl:recv(Socket, Length, ?TCP_TIMEOUT)}) end),
    {noreply, State#client{reference = Pid}}.

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
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [?PACKET_HEADER_LENGTH + byte_size(Binary), Protocol, Binary]),
            async_receive(Read, State#client{protocol = 0})
    end.
