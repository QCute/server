%%%-------------------------------------------------------------------
%%% @doc
%%% account control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([query/3, create/10, login/3, logout/1, heartbeat/1, handle_packet/2]).
%% Includes
-include("net.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("online.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc account query
-spec query(State :: #client{}, ServerId :: non_neg_integer(), Account :: binary()) -> {ok, #client{}}.
query(State, ServerId, AccountName) ->
    Result = sql:select_column(parser:format(<<"SELECT `role_name` FROM `role` WHERE `server_id` = ~w AND `account_name` = '~s'">>, [ServerId, AccountName])),
    {ok, QueryResponse} = user_router:write(?PROTOCOL_ACCOUNT_QUERY, Result),
    sender:send(State, QueryResponse),
    {ok, State}.

%% @doc account create
-spec create(State :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary(), RoleName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), Channel :: binary(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}}.
create(State, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case check_server_state(State, ServerId) of
        {ok, _} ->
            check_create_name(State, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
        Error ->
            Error
    end.

check_create_name(State, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    %% validate name word length and sensitive and duplicate
    case word:validate(RoleName, [{length, 1, 6}, sensitive, {sql, parser:format(<<"SELECT `account_name` FROM `role` WHERE `role_name` = '~s'">>, [RoleName])}]) of
        true ->
            Result = start_create(State, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
        {false, length, _} ->
            Result = name_length;
        {false, asn1, _} ->
            Result = name_not_utf8;
        {false, sensitive} ->
            Result = name_sensitive;
        {false, duplicate} ->
            Result = name_duplicate
    end,
    {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, Result),
    sender:send(State, CreateResponse),
    {ok, State}.

start_create(#client{ip = IP}, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    Now = time:now(),
    Role = #role{
        role_name = RoleName,
        server_id = ServerId,
        account_name = AccountName,
        sex = Sex,
        classes = Classes,
        status = ?SERVER_STATE_NORMAL,
        online = 1,
        register_time = Now,
        login_time = Now,
        item_size = parameter_data:get(item_size),
        bag_size = parameter_data:get(bag_size),
        store_size = parameter_data:get(store_size),
        channel = Channel,
        device_id = DeviceId,
        device_type = DeviceType,
        mac = Mac,
        ip = list_to_binary(inet_parse:ntoa(IP))
    },
    %% name will duplicate
    case role_sql:insert(Role) of
        {'EXIT', _} ->
            name_duplicate;
        _ ->
            ok
    end.

%% @doc account login
-spec login(State :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(State, ServerId, AccountName) ->
    case check_server_state(State, ServerId) of
        {ok, ServerState} ->
            check_user_login(State, ServerId, AccountName, ServerState);
        Error ->
            Error
    end.

check_user_login(State, ServerId, AccountName, ServerState) ->
    case sql:select(parser:format(<<"SELECT `role_id`, `role_name`, `status`, `logout_time` FROM `role` WHERE `account_name` = '~s'">>, [AccountName])) of
        [[RoleId, RoleName, Status, LogoutTime]] ->
            check_user_permission(State, RoleId, RoleName, ServerId, AccountName, Status, LogoutTime, ServerState);
        _ ->
            %% cannot find role
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, no_such_name),
            sender:send(State, LoginResponse),
            {stop, normal, State}
    end.

check_user_permission(State, RoleId, RoleName, ServerId, AccountName, Status, LogoutTime, ServerState) ->
    %% refuse role login
    case Status band ?SERVER_STATE_REFUSE =/= 0 of
        true ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        false when ServerState band Status == 0 ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, permission_denied),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        false ->
            start_login(State, RoleId, RoleName, ServerId, AccountName, LogoutTime)
    end.

%% common login
start_login(State = #client{socket = Socket, protocol_type = ProtocolType}, RoleId, RoleName, ServerId, AccountName, LogoutTime) ->
    %% new login
    case user_server:start(RoleId, RoleName, ServerId, AccountName, LogoutTime, self(), Socket, ProtocolType) of
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
-spec logout(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
logout(State = #client{role_pid = RolePid}) ->
    %% notify user server logout
    user_server:cast(RolePid, {stop, logout}),
    {ok, LogoutResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, ok),
    sender:send(State, LogoutResponse),
    %% stop receiver
    {stop, normal, State}.

%% @doc heart beat
-spec heartbeat(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
heartbeat(State) ->
    %% heart packet check
    Now = time:now(),
    case Now < State#client.heartbeat_time + 30 of
        true ->
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, heartbeat_packet_fast_error),
            sender:send(State, Response),
            {stop, normal, State};
        _ ->
            {ok, State#client{heartbeat_time = Now}}
    end.

%% @doc handle packet and packet speed control
-spec handle_packet(State :: #client{}, Data :: [term()]) -> {ok, #client{}} | {stop, term(), #client{}}.
handle_packet(State = #client{protocol = Protocol, role_pid = Pid, total_packet = Total, last_time = LastTime}, Data) ->
    Now = time:now(),
    case Total < 120 of
        true when is_pid(Pid) ->
            %% normal game data
            user_server:socket_event(Pid, Protocol, Data),
            {ok, State#client{total_packet = Total + 1, last_time = Now}};
        false when LastTime + 10 < Now andalso is_pid(Pid) ->
            %% normal game data
            user_server:socket_event(Pid, Protocol, Data),
            {ok, State#client{total_packet = 0, last_time = Now}};
        _ ->
            %% 1 seconds 10 packets
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, packet_fast_error),
            sender:send(State, Response),
            {stop, normal, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_server_state(State, ServerId) ->
    %% server control state
    ThisServerId = config:server_id(),
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        ServerState when ServerState band ?SERVER_STATE_REFUSE =/= 0 ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        _ when ServerId =/= ThisServerId ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, server_id_not_match),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        ServerState ->
            {ok, ServerState}
    end.
