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
%%% API
%%%===================================================================
%% @doc data handle
handle(State = #client{state = wait_pack_first}, Data) ->
    case Data of
        <<"GET ">> ->
            {read, 6, ?HEART_TIMEOUT, State#client{state = wait_http_first, http_header = Data}};
        <<"POST">> ->
            {read, 7, ?HEART_TIMEOUT, State#client{state = wait_http_first, http_header = Data}};
        <<Length:16, Protocol:16>> ->
            read_tcp(State#client{connect_type = tcp}, Length - 4, Protocol)
    end;

handle(State = #client{state = wait_tcp_head}, <<Length:16, Protocol:16>>) ->
    read_tcp(State, Length - 2, Protocol);

handle(State = #client{state = wait_tcp_pack}, Data) ->
    case dispatch(State, Data) of
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, Error, NewState} ->
            {stop, Error, NewState};
        Reason ->
            {stop, {wait_tcp_pack, {unknown_state_return, Reason}}, State}
    end;
handle(State = #client{state = wait_http_first, http_header = Header}, Data) ->
    case <<Header/binary, Data/binary>> of
        <<"GET / HTTP">> ->
            %% request root must
            {read, 0, ?HEART_TIMEOUT, State#client{state = treat_html5_request, http_header = <<Header/binary, Data/binary>>}};
        <<"POST / HTTP">> ->
            %% request root must
            {read, 0, ?HEART_TIMEOUT, State#client{state = treat_html5_request, http_header = <<Header/binary, Data/binary>>}};
        Binary ->
            {stop, {wait_http_first, http_request_normal, Binary}, State}
    end;

handle(State = #client{state = treat_html5_request, http_header = HttpHeader}, Data) ->
    web_socket:handle_http_head(<<HttpHeader/binary, Data/binary>>, State#client{http_header = <<>>});

handle(State = #client{state = wait_html5_head}, Data) ->
    web_socket:handle_html5_head(Data, State);

handle(State = #client{state = wait_html5_body_length}, Data) ->
    web_socket:handle_html5_body_length(Data, State);

handle(State = #client{state = wait_html5_body, packet = Packet}, Data) ->
    %% decode continue packet
    {PayLoad, Read, NextState} = web_socket:decode(State, Data),
    read_http(State#client{state = NextState, packet = <<>>}, Read, <<Packet/binary, PayLoad/binary>>);
handle(State, _) ->
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% read tcp protocol
read_tcp(State, Length, Protocol) when Length =< 0 ->
    case dispatch(State#client{protocol = Protocol}, <<>>) of
        ok ->
            {continue, State};
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, ErrorCode, NewState} ->
            {stop, ErrorCode, NewState};
        Reason ->
            {stop, {unknown_state_return, Reason}, State}
    end;
read_tcp(State, Length, Protocol) when Length > 0 ->
    {read, Length, ?TCP_TIMEOUT, State#client{state = wait_tcp_pack, protocol = Protocol}};
read_tcp(State, _, _) ->
    {continue, State}.

%% read http protocol
read_http(State, NextRead, <<Length:16, Protocol:16, Binary/binary>>) when Length - 4 =< size(Binary) ->
    DataLength = Length - 4,
    <<BinaryData:DataLength/binary, RemainBinary/binary>> = Binary,
    case dispatch(State#client{packet_length = Length, protocol = Protocol}, BinaryData) of
        {ok, NewState} ->
            %% multi protocol in one packet
            read_http(NewState, NextRead, RemainBinary);
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        Reason ->
            {stop, {unknown_state_return, Reason}, State}
    end;
read_http(State, NextRead, <<>>) ->
    %% one protocol in one packet
    {read, NextRead, ?TCP_TIMEOUT, State};
read_http(State, NextRead, Binary) ->
    %% one protocol in one packet, but receive packet not complete
    {read, NextRead, ?TCP_TIMEOUT, State#client{packet = Binary}}.

%%% handle packet data
dispatch(State = #client{login_state = LoginState, protocol = Protocol, user_pid = Pid}, Binary) ->
    %% protocol dispatch
    try
        {ok, Data} = player_route:read(Protocol, Binary),
        %% common game data
        _ = LoginState == login andalso gen_server:cast(Pid, {'SOCKET_EVENT', Protocol, Data}) == ok,
        %% common game data
        case account_handle:handle(Protocol, State, Data) of
            ok ->
                {ok, State};
            Return ->
                Return
        end
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ?DEBUG("~n~p~n", [<<(State#client.packet_length):16, Protocol:16, Binary/binary>>]),
        {ok, State}
    end.
