%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([query/3, create/10, login/3, logout/3, heartbeat/1, handle_packet/2]).
%% Includes
-include("net.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("online.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
-spec query(State :: #client{}, ServerId :: non_neg_integer(), Account :: binary()) -> {ok, #client{}}.
query(State, ServerId, Account) ->
    case sql:select(parser:format(<<"SELECT 1 FROM `role` WHERE server_id = ~w AND `account` = '~s'">>, [ServerId, Account])) of
        [[1]] ->
            {ok, QueryResponse} = user_router:write(?PROTOCOL_ACCOUNT_QUERY, ok),
            sender:send(State, QueryResponse);
        [] ->
            {ok, QueryResponse} = user_router:write(?PROTOCOL_ACCOUNT_QUERY, no_such_account),
            sender:send(State, QueryResponse)
    end,
    {ok, State}.

%% @doc create account
-spec create(State :: #client{}, ServerId :: non_neg_integer(), Account :: binary(), RoleName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), Channel :: binary(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}}.
create(State = #client{ip = IP}, ServerId, Account, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case word:validate(RoleName, [{length, 1, 6}, sensitive, {sql, parser:format(<<"SELECT `role_id` FROM `role` WHERE `account` = '~s'">>, [Account])}]) of
        true ->
            Now = time:ts(),
            Role = #role{
                server_id = ServerId,
                account = Account,
                role_name = RoleName,
                type = ?SERVER_STATE_NORMAL,
                sex = Sex,
                classes = Classes,
                item_size = parameter_data:get(item_size),
                bag_size = parameter_data:get(bag_size),
                store_size = parameter_data:get(store_size),
                online = 1,
                online_time = Now,
                register_time = Now,
                channel = Channel,
                device_id = DeviceId,
                device_type = DeviceType,
                mac = Mac,
                ip = list_to_binary(inet_parse:ntoa(IP))
            },
            role_sql:insert(Role),
            Result = ok;
        {false, length, _} ->
            Result = duplicate;
        {false, asn1, _} ->
            Result = not_utf8;
        {false, sensitive} ->
            Result = sensitive;
        {false, duplicate} ->
            Result = duplicate
    end,
    {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, Result),
    sender:send(State, CreateResponse),
    {ok, State}.

%% @doc account login
-spec login(State :: #client{}, ServerId :: non_neg_integer(), Account :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(State, ServerId, Account) ->
    ThisServerId = config:server_id(),
    %% check account/infant/blacklist etc...
    case sql:select(io_lib:format("SELECT `role_id` FROM `role` WHERE `account` = '~s'", [Account])) of
        [[RoleId]] when ServerId == ThisServerId ->
            %% only one match user id
            %% start user process check reconnect first
            check_user_type(State, RoleId);
        [[_]] ->
            %% failed result reply
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, server_id_not_match),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        _ ->
            %% failed result reply
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, no_such_name),
            sender:send(State, LoginResponse),
            {stop, normal, State}
    end.

check_user_type(State = #client{}, RoleId) ->
    %% control server open or not
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        ?SERVER_STATE_REFUSE ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        ServerState ->
            case sql:select(parser:format(<<"SELECT 1 FROM `role` WHERE `role_id` = ~w and `type` >= ~w">>, [RoleId, ServerState])) of
                [] ->
                    {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, permission_denied),
                    sender:send(State, LoginResponse),
                    {stop, normal, State};
                _ ->
                    start_login(State, RoleId)
            end
    end.

%% common login
start_login(State = #client{socket = Socket, protocol_type = ProtocolType}, RoleId) ->
    %% new login
    case user_server:start(RoleId, self(), Socket, ProtocolType) of
        {ok, Pid} ->
            {ok, State#client{login_state = login, role_id = RoleId, role_pid = Pid}};
        {error, {already_started, Pid}} ->
            %% reconnect
            gen_server:cast(Pid, {reconnect, self(), Socket, ProtocolType}),
            {ok, State#client{login_state = login, role_id = RoleId, role_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.

%% @doc account logout
-spec logout(State :: #client{}, ServerId :: non_neg_integer(), Account :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
logout(State, ServerId, Account) ->
    ThisServerId = config:server_id(),
    %% check account/infant/blacklist etc...
    case sql:select(io_lib:format("SELECT `role_id` FROM `role` WHERE `account` = '~s'", [Account])) of
        [[_]] when ServerId == ThisServerId ->
            {stop, normal, State};
        [[_]] ->
            %% failed result reply
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, server_id_not_match),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        _ ->
            %% failed result reply
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, no_such_name),
            sender:send(State, LoginResponse),
            {stop, normal, State}
    end.

%% @doc heart beat
-spec heartbeat(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
heartbeat(State) ->
    %% heart packet check
    Now = time:ts(),
    case Now < State#client.heartbeat_time + 30 of
        true ->
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, heartbeat_packet_fast_error),
            sender:send(State, Response),
            {stop, normal, State};
        _ ->
            {ok, State#client{heartbeat_time = Now}}
    end.

%% @doc handle packet and packet speed control
-spec handle_packet(State :: #client{}, Data :: [term()]) -> {ok, #client{}} | {stop, term(), #client{}}.
handle_packet(State = #client{protocol = Protocol, role_pid = Pid, total_packet = Total, last_time = LastTime}, Data) ->
    Now = time:ts(),
    case 10 < Total of
        true when Now < LastTime + 1 ->
            %% 1 seconds 10 packets
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, packet_fast_error),
            sender:send(State, Response),
            {stop, normal, State};
        true ->
            %% normal game data
            user_server:socket_event(Pid, Protocol, Data),
            {ok, State#client{total_packet = 0, last_time = Now}};
        false ->
            %% normal game data
            user_server:socket_event(Pid, Protocol, Data),
            {ok, State#client{total_packet = Total + 1, last_time = Now}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
