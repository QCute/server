%%%-------------------------------------------------------------------
%%% @doc
%%% account control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([query/3, create/10, login/5, logout/1, heartbeat/1, handle_packet/2]).
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
    Result = db:select(parser:format(<<"SELECT `role_id`, `role_name` FROM `role` WHERE `origin_server_id` = ~w AND `account_name` = '~s'">>, [ServerId, AccountName])),
    {ok, QueryResponse} = user_router:write(?PROTOCOL_ACCOUNT_QUERY, [list_to_tuple(Row) || Row <- Result]),
    sender:send(State, QueryResponse),
    {ok, State}.

%% @doc account create
-spec create(State :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary(), RoleName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), Channel :: binary(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}}.
create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case check_server_state(State, ServerId) of
        {ok, _} ->
            Result = create_check_sex(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType),
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, Result),
            sender:send(State, CreateResponse),
            {ok, State};
        Error ->
            Error
    end.

create_check_sex(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case role_data:sex(Sex) of
        <<>> ->
            [invalid_sex, 0, <<>>];
        _ ->
            create_check_classes(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_classes(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case role_data:classes(Classes) of
        <<>> ->
            [invalid_classes, 0, <<>>];
        _ ->
            create_check_name(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_name(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    %% validate name word length and sensitive and duplicate
    case word:validate(RoleName, [{length, 1, 6}, sensitive, {sql, parser:format(<<"SELECT `role_name` FROM `role` WHERE `role_name` = '~s' OR `account_name` = '~s'">>, [RoleName, AccountName])}]) of
        true ->
            start_create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
        {false, length, _} ->
            [name_length, 0, <<>>];
        {false, asn1, _} ->
            [name_not_utf8, 0, <<>>];
        {false, sensitive} ->
            [name_sensitive, 0, <<>>];
        {false, duplicate, [[RoleName]]} ->
            [name_duplicate, 0, <<>>];
        {false, duplicate, _} ->
            [duplicate, 0, <<>>]
    end.

start_create(#client{ip = IP}, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    Now = time:now(),
    Role = #role{
        role_name = RoleName,
        server_id = ServerId,
        account_name = AccountName,
        origin_server_id = ServerId,
        sex = Sex,
        classes = Classes,
        type = ?SERVER_STATE_NORMAL,
        is_online = 1,
        register_time = Now,
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
    case catch role_sql:insert(Role) of
        {'EXIT', _} ->
            [name_duplicate, 0, <<>>];
        RoleId when is_integer(RoleId) ->
            [ok, RoleId, RoleName]
    end.

%% @doc account login
-spec login(State :: #client{}, RoleId :: non_neg_integer(), RoleName :: binary(), ServerId :: non_neg_integer(), AccountName :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(State, RoleId, RoleName, ServerId, AccountName) ->
    case check_server_state(State, ServerId) of
        {ok, ServerState} ->
            login_check_user(State, RoleId, RoleName, ServerId, AccountName, ServerState);
        Error ->
            Error
    end.

login_check_user(State, RoleId, RoleName, ServerId, AccountName, ServerState) ->
    case db:select(parser:format(<<"SELECT `type`, `logout_time` FROM `role` WHERE `role_id` = ~w AND `account_name` = '~s'">>, [RoleId, AccountName])) of
        [[Type, LogoutTime]] ->
            login_check_permission(State, RoleId, RoleName, ServerId, AccountName, Type, LogoutTime, ServerState);
        _ ->
            %% cannot find role
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, no_account),
            sender:send(State, LoginResponse),
            {stop, normal, State}
    end.

login_check_permission(State, RoleId, RoleName, ServerId, AccountName, Type, LogoutTime, ServerState) ->
    %% refuse role login
    case Type == ?SERVER_STATE_REFUSE of
        true ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, LoginResponse),
            {stop, normal, State};
        false when ServerState > Type ->
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
            {ok, State#client{role_pid = Pid}};
        {error, {already_started, Pid}} ->
            %% reconnect
            gen_server:cast(Pid, {reconnect, self(), Socket, ProtocolType}),
            {ok, State#client{role_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.

%% @doc account logout
-spec logout(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
logout(State = #client{role_pid = Pid}) ->
    %% notify user server logout
    is_pid(Pid) andalso user_server:cast(Pid, {stop, logout}),
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
handle_packet(State = #client{protocol = Protocol, role_pid = Pid}, Data) ->
    case user_router:interval(State) of
        {true, NewState} ->
            %% normal game data
            is_pid(Pid) andalso user_server:socket_event(Pid, Protocol, Data),
            {ok, NewState};
        {false, NewState} ->
            {ok, Response} = user_router:write(Protocol, packet_too_fast),
            sender:send(State, Response),
            {ok, NewState}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_server_state(State, ServerId) ->
    %% server control state
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, Response),
            {stop, normal, State};
        ?SERVER_STATE_REFUSE ->
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, refuse),
            sender:send(State, Response),
            {stop, normal, State};
        ServerState ->
            case ServerId == config:server_id() orelse lists:member(ServerId, config:server_id_list()) of
                false ->
                    {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, server_id_not_match),
                    sender:send(State, Response),
                    {stop, normal, State};
                true ->
                    {ok, ServerState}
            end
    end.
