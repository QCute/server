%%%-------------------------------------------------------------------
%%% @doc
%%% module reader
%%% @end
%%%-------------------------------------------------------------------
-module(reader).
-include("common.hrl").
-include("socket.hrl").
-include("protocol.hrl").
%% export API function
-export([handle/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc data handle
handle(State = #client{state = wait_pack_first}, Data) ->
    case Data of
        <<"GET ">> ->
            {read, ?PACKET_HEAD_LENGTH, ?HEART_TIMEOUT, State#client{state = wait_http_first, connect_type = http}};
        <<"POST">> ->
            {read, ?PACKET_HEAD_LENGTH, ?HEART_TIMEOUT, State#client{state = wait_http_first, connect_type = http}};
        <<Length:16, Protocol:16>> ->
            read(State#client{connect_type = tcp}, Length - 4, Protocol)
    end;

handle(State = #client{state = wait_tcp_head}, <<Length:16, Protocol:16>>) ->
    read(State, Length - 2, Protocol);

handle(State = #client{state = wait_tcp_pack}, Data) ->
    case dispatch(State, Data) of
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, Error, State} ->
            {stop, Error, State};
        _ ->
            {stop, {wait_tcp_pack, unknown_state_return}, State}
    end;
handle(State = #client{state = wait_http_first}, Data) ->
    case Data of
        <<"/ HT">> ->
            {read, 0, ?HEART_TIMEOUT, State#client{state = treat_html5_request, packet = Data}};
        _ ->
            {stop, {wait_http_first, http_request_normal}, State}
    end;

handle(State = #client{state = treat_html5_request, packet = Packet}, Data) ->
    web_socket:handle_http_head(<<Packet/binary, Data/binary>>, State#client{packet = <<>>});

handle(State = #client{state = wait_html5_head}, Data) ->
    web_socket:handle_html5_head(Data, State);

handle(State = #client{state = wait_html5_body_length}, Data) ->
    web_socket:handle_html5_body_length(Data, State);

handle(State = #client{state = wait_html5_body, packet_type = raw}, Data) ->
    %% 解码
    {PayLoad, Read, NextState} = web_socket:decode(State, Data),
    console:print(?MODULE, ?LINE, "~p~n", [PayLoad]),
    {read, Read, ?TCP_TIMEOUT, State#client{state = NextState, packet_type = 0}};
handle(State = #client{state = wait_html5_body}, Data) ->
    %% 解码
    {PayLoad, Read, NextState} = web_socket:decode(State, Data),
    <<Length:16, Protocol:16, BinaryData/binary>> = PayLoad,
    case dispatch(State#client{packet_length = Length, protocol = Protocol}, BinaryData) of
        {ok, NewState} ->
            {read, Read, ?TCP_TIMEOUT, NewState#client{state = NextState}};
        {stop, Error, State} ->
            {stop, Error, State};
        _ ->
            {stop, {wait_html5_body, unknown_state_return}, State}
    end;
handle(State, _) ->
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% read protocol
read(State, Length, Protocol) when Length =< 0 ->
    case dispatch(State#client{protocol = Protocol}, <<>>) of
        ok ->
            {continue, State};
        {ok, NewState} ->
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, ErrorCode, State} ->
            {stop, ErrorCode, State};
        _ ->
            {stop, unknown_state_return, State}
    end;
read(State, Length, Protocol) when Length > 0 ->
    {read, Length, ?TCP_TIMEOUT, State#client{state = wait_tcp_pack, protocol = Protocol}};
read(State, _, _) ->
    {continue, State}.

%%% handle packet data
dispatch(State = #client{login_state = LoginState, packet_length = Length, protocol = Protocol, user_pid = Pid}, Binary) ->
    %% 协议分发
    try
        {ok, Data} = player_route:read(Protocol, Binary),
        %% common game data
        _ = LoginState == login andalso gen_server:cast(Pid, {'SOCKET_EVENT', Protocol, Data}) == ok,
        %% common game data
        account_handle:handle(Protocol, State, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        console:print(?MODULE, ?LINE, "~n~p~n", [<<Length:16, Protocol:16, Binary/binary>>]),
        {ok, State}
    end.
