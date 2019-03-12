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
            {read, 0, ?HEART_TIMEOUT, State#client{state = wait_http_head}};
        <<"POST">> ->
            {read, 0, ?HEART_TIMEOUT, State#client{state = wait_http_head}};
        <<Length:16, Protocol:16>> ->
            read(State, Length - 4, Protocol)
    end;
handle(State = #client{state = wait_tcp_head}, <<Length:16, Protocol:16>>) ->
    read(State, Length - 2, Protocol);
handle(State = #client{state = wait_tcp_pack}, Data) ->
    case dispatch(State, Data) of
        {ok, NewState} ->
            {read, ?PACK_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, Error, State} ->
            {stop, Error, State};
        _ ->
            {stop, {wait_tcp_pack, ?UNKNOWN_STATE_RETURN}, State}
    end;
handle(State = #client{state = wait_http_head}, Data) ->
    web_socket:handle_http_head(Data, State);
handle(State = #client{state = wait_html5_head}, Data) ->
    web_socket:handle_html5_head(Data, State);
handle(State = #client{state = wait_html5_body_length}, Data) ->
    web_socket:handle_html5_body_length(Data, State);
handle(State = #client{state = wait_html5_body, protocol_type = ?PROTOCOL_TYPE_WS_HYBI}, Data) ->
    %% 处理掩码
    PayLoad = web_socket:unmask(Data, State#client.masking_h5),
    <<_Length:16, Protocol:16, BinaryData/binary>> = PayLoad,
    case dispatch(State#client{protocol = Protocol}, BinaryData) of
        {ok, NewState} ->
            {read, 0, ?TCP_TIMEOUT, NewState#client{state = wait_html5_body}};
        {stop, Error, State} ->
            {stop, Error, State};
        _ ->
            {stop, {wait_html5_body, ?UNKNOWN_STATE_RETURN}, State}
    end;
handle(State = #client{state = wait_html5_body, protocol_type = ?PROTOCOL_TYPE_WS_HIXIE}, Data) ->
    %% 处理掩码
    PayLoad = web_socket:frames(Data, []),
    <<_Length:16, Protocol:16, BinaryData/binary>> = list_to_binary(PayLoad),
    case dispatch(State#client{protocol = Protocol}, BinaryData) of
        {ok, NewState} ->
            {read, 0, ?TCP_TIMEOUT, NewState#client{state = wait_html5_body}};
        {stop, Error, State} ->
            {stop, Error, State};
        _ ->
            {stop, {wait_html5_body, ?UNKNOWN_STATE_RETURN}, State}
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
            {read, ?PACK_HEAD_LENGTH, ?TCP_TIMEOUT, NewState#client{state = wait_tcp_head}};
        {stop, ErrorCode, State} ->
            {stop, ErrorCode, State};
        _ ->
            {stop, ?UNKNOWN_STATE_RETURN, State}
    end;
read(State, Length, Protocol) when Length > 0 ->
    {read, Length, ?TCP_TIMEOUT, State#client{state = wait_tcp_pack, protocol = Protocol}};
read(State, _, _) ->
    {continue, State}.

%%% handle packet data
dispatch(State = #client{login_state = LoginState, protocol = Protocol, user_pid = Pid}, Binary) ->
    %% 协议分发
    try
        {ok, Data} = player_route:read(Protocol, Binary),
        %% common game data
        case LoginState of
            login ->
                gen_server:cast(Pid, {'SOCKET_EVENT', Protocol, Data});
            _ ->
                ok
        end,
        %% common game data
        account_handle:handle(Protocol, State, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {ok, State}
    end.
