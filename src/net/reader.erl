%%%-------------------------------------------------------------------
%%% @doc
%%% module reader
%%% @end
%%%-------------------------------------------------------------------
-module(reader).
%% API
-export([handle/2]).
%% Includes
-include("common.hrl").
-include("socket.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc data handle
-spec handle(State :: #client{}, Data :: binary()) -> {continue, #client{}} | {read, non_neg_integer(), non_neg_integer(), #client{}} | {stop, term(), #client{}}.
handle(State = #client{state = wait_pack_first}, Data) ->
    case Data of
        <<"GET ">> ->
            {read, 8, ?TCP_TIMEOUT, State#client{state = wait_http_first, http_header = Data}};
        <<"POST">> ->
            {read, 8, ?TCP_TIMEOUT, State#client{state = wait_http_first, http_header = Data}};
        <<"HEAD">> ->
            {read, 8, ?TCP_TIMEOUT, State#client{state = wait_http_first, http_header = Data}};
        <<Length:16, Protocol:16>> ->
            read_tcp(State#client{protocol_type = tcp}, Length - 4, Protocol)
    end;

handle(State = #client{state = wait_tcp_head}, <<Length:16, Protocol:16>>) ->
    read_tcp(State, Length - 2, Protocol);

handle(State = #client{state = wait_tcp_pack}, Data) ->
    case dispatch(State, Data) of
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, Error, NewState} ->
            {stop, Error, NewState}
    end;

handle(State = #client{state = wait_http_first, http_header = HttpHeader}, Data) ->
    case <<HttpHeader/binary, Data/binary>> of
        Header = <<"GET / HTTP/", _/binary>> ->
            %% request root must
            {read, 0, ?TCP_TIMEOUT, State#client{state = treat_html5_request, http_header = Header}};
        Header = <<"POST / HTTP/">> ->
            %% request root must
            {read, 0, ?TCP_TIMEOUT, State#client{state = treat_html5_request, http_header = Header}};
        Header = <<"HEAD / HTTP/">> ->
            %% request root must
            {read, 0, ?TCP_TIMEOUT, State#client{state = treat_html5_request, http_header = Header}};
        Binary ->
            %% reject other invalid request
            {stop, {shutdown, {wait_http_first, Binary}}, State}
    end;

handle(State = #client{state = treat_html5_request, http_header = HttpHeader}, Data) ->
    Http = http:parse_content(<<HttpHeader/binary, Data/binary>>),
    handle_http(Http, State#client{http_header = <<>>});

handle(State = #client{state = wait_html5_head}, Data) ->
    web_socket:handle_html5_head(Data, State);

handle(State = #client{state = wait_html5_body_length}, Data) ->
    web_socket:handle_html5_body_length(Data, State);

handle(State = #client{state = wait_html5_body, packet = Packet}, Data) ->
    %% decode continue packet
    {PayLoad, Read, NextState} = web_socket:decode(Data, State),
    read_http(State#client{state = NextState, packet = <<>>}, Read, <<Packet/binary, PayLoad/binary>>);

handle(State, _) ->
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% read tcp protocol
read_tcp(State, 0, Protocol) ->
    case dispatch(State#client{protocol = Protocol}, <<>>) of
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, ErrorCode, NewState} ->
            {stop, ErrorCode, NewState}
    end;
read_tcp(State, Length, Protocol) when Length > 0 ->
    {read, Length, ?TCP_TIMEOUT, State#client{state = wait_tcp_pack, protocol = Protocol}};
read_tcp(State, _, _) ->
    {continue, State}.

%% read http protocol
read_http(State, NextRead, <<Length:16, Protocol:16, Binary/binary>>) when Length - 4 =< byte_size(Binary) ->
    DataLength = Length - 4,
    <<BinaryData:DataLength/binary, RemainBinary/binary>> = Binary,
    case dispatch(State#client{packet_length = Length, protocol = Protocol}, BinaryData) of
        {ok, NewState} ->
            %% multi protocol in one packet
            read_http(NewState, NextRead, RemainBinary);
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end;
read_http(State, NextRead, <<>>) ->
    %% one protocol in one packet
    {read, NextRead, ?TCP_TIMEOUT, State};
read_http(State, NextRead, Binary) ->
    %% one protocol in one packet, but the received packet not complete
    {read, NextRead, ?TCP_TIMEOUT, State#client{packet = Binary}}.

%%% handle packet data
dispatch(State = #client{protocol = Protocol}, Binary) ->
    %% decode protocol data
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            try
                %% protocol dispatch
                account_handler:handle(Protocol, State, Data)
            catch ?EXCEPTION(_Class, _Reason, _Stacktrace) ->
                ?STACKTRACE(_Reason, _Stacktrace),
                {ok, State}
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [(State#client.packet_length), Protocol, Binary]),
            {ok, State}
    end.

%% handle http request
%% GET / HTTP/1.1\r\n
%% POST / HTTP/1.1\r\n
%% HEAD / HTTP/1.1\r\n
handle_http(#http{method = <<"HEAD">>, version = Version}, State) ->
    Response = [
        <<"HTTP/">>, Version, <<" 200 OK\r\n">>,
        <<"Connection: close\r\n">>,
        <<"Date: ">>, list_to_binary(httpd_util:rfc1123_date()), <<"\r\n">>,
        <<"Server: erlang/">>, list_to_binary(erlang:system_info(version)), <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Response)),
    {stop, normal, State};
handle_http(Http, State) ->
    case http:get_header_field(<<"Upgrade">>, Http) of
        <<"websocket">> ->
            %% web socket upgrade
            web_socket:handle_upgrade(Http, State);
        _ ->
            %% game master http command
            handle_http_command(Http, State)
    end.

handle_http_command(Http, State = #client{socket_type = gen_tcp, socket = Socket}) ->
    case inet:peername(Socket) of
        {ok, {{127, 0, 0, 1}, _Port}} ->
            %% ipv4 local loop address
            master:treat(State, Http),
            {stop, normal, State};
        {ok, {{0, 0, 0, 0, 0, 0, 16#7f00, 16#01}, _Port}} ->
            %% ipv6 local loop address
            master:treat(State, Http),
            {stop, normal, State};
        {ok, _} ->
            %% other ip address, ignore it
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_http_command(Http, State = #client{socket_type = ssl, socket = Socket}) ->
    case ssl:peername(Socket) of
        {ok, {{127, 0, 0, 1}, _Port}} ->
            %% ipv4 local loop address
            master:treat(State, Http),
            {stop, normal, State};
        {ok, {{0, 0, 0, 0, 0, 0, 16#7f00, 16#01}, _Port}} ->
            %% ipv6 local loop address
            master:treat(State, Http),
            {stop, normal, State};
        {ok, _} ->
            %% other ip address, ignore it
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end.
