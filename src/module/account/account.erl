%%%-------------------------------------------------------------------
%%% @doc
%%% account control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([query/3, create/10, login/5, logout/1, heartbeat/1, handle_packet/3]).
-export([version/0]).
%% Includes
-include("common.hrl").
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
    Result = db:select(<<"SELECT `role_id`, `role_name` FROM `role` WHERE `origin_server_id` = ~w AND `account_name` = '~s'">>, [ServerId, AccountName]),
    {ok, QueryResponse} = user_router:write(?PROTOCOL_ACCOUNT_QUERY, [list_to_tuple(Row) || Row <- Result]),
    sender:send(State, QueryResponse),
    {ok, State}.

%% @doc account create
-spec create(State :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary(), RoleName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), Channel :: binary(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}}.
create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case create_check_server_id(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) of
        {ok, RoleId} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [ok, RoleId, RoleName]),
            sender:send(State, CreateResponse),
            {ok, State};
        {error, Reason} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [Reason, 0, <<>>]),
            sender:send(State, CreateResponse),
            {stop, normal, State}
    end.

create_check_server_id(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case ServerId == config:server_id() orelse lists:keymember(ServerId, 1, config:server_id_list()) of
        false ->
            {error, server_id_mismatch};
        true ->
            create_check_state(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_state(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case user_manager:get_create_state() of
        ?FALSE ->
            {error, server_create_forbidden};
        _ ->
            create_check_limit(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_limit(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case db:select_one(<<"SELECT `TABLE_ROWS` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = 'role'">>) >= db:limit() of
        true ->
            {error, account_create_max};
        _ ->
            create_check_sex(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_sex(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case role_data:sex(Sex) of
        <<>> ->
            {error, invalid_sex};
        _ ->
            create_check_classes(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_classes(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case role_data:classes(Classes) of
        <<>> ->
            {error, invalid_classes};
        _ ->
            create_check_name(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_name(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    %% validate name word length and sensitive and duplicate
    case word:validate(RoleName, [{length, 1, 6}, sensitive, {sql, parser:format(<<"SELECT `role_name` FROM `role` WHERE `role_name` = '~s' OR `account_name` = '~s'">>, [RoleName, AccountName])}]) of
        true ->
            start_create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
        {false, length, _} ->
            {error, name_length_invalid};
        {false, asn1, _} ->
            {error, name_not_utf8_charset};
        {false, sensitive} ->
            {error, name_sensitive};
        {false, duplicate, [[RoleName]]} ->
            {error, name_duplicated};
        {false, duplicate, _} ->
            {error, name_duplicated}
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
            {error, name_duplicate};
        RoleId when is_integer(RoleId) ->
            {ok, RoleId}
    end.

%% @doc account login
-spec login(State :: #client{}, RoleId :: non_neg_integer(), RoleName :: binary(), ServerId :: non_neg_integer(), AccountName :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(State, RoleId, RoleName, ServerId, AccountName) ->
    case check_server_id(State, RoleId, RoleName, ServerId, AccountName) of
        {ok, Pid} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, ok),
            sender:send(State, CreateResponse),
            State#client{role_pid = Pid};
        {error, Reason} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, Reason),
            sender:send(State, CreateResponse),
            {stop, normal, State};
        {stop, Reason} ->
            {stop, Reason, State}
    end.

check_server_id(State, RoleId, RoleName, ServerId, AccountName) ->
    case ServerId == config:server_id() orelse lists:keymember(ServerId, 1, config:server_id_list()) of
        false ->
            {error, server_id_mismatch};
        true ->
            check_server_state(State, RoleId, RoleName, ServerId, AccountName)
    end.

check_server_state(State, RoleId, RoleName, ServerId, AccountName) ->
    %% server control state
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            {error, server_login_forbidden};
        ?SERVER_STATE_FORBIDDEN ->
            {error, server_login_forbidden};
        ServerState ->
            login_check_user(State, RoleId, RoleName, ServerId, AccountName, ServerState)
    end.

login_check_user(State, RoleId, RoleName, ServerId, AccountName, ServerState) ->
    case db:select(parser:format(<<"SELECT `type` FROM `role` WHERE `role_id` = ~w AND `account_name` = '~s'">>, [RoleId, AccountName])) of
        [] ->
            %% cannot find role
            {error, account_not_found};
        [[?SERVER_STATE_FORBIDDEN]] ->
            %% refuse role login
            {error, account_login_forbidden};
        [[Type]] when Type < ServerState ->
            %% permission denied
            {error, account_permission_denied};
        [[_]] ->
            start_login(State, RoleId, RoleName, ServerId, AccountName)
    end.

%% common login
start_login(#client{socket = Socket, protocol_type = ProtocolType}, RoleId, RoleName, ServerId, AccountName) ->
    %% new login
    case user_server:start(RoleId, RoleName, ServerId, AccountName, self(), Socket, ProtocolType) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            %% reconnect
            gen_server:cast(Pid, {reconnect, self(), Socket, ProtocolType}),
            {ok, Pid};
        Error ->
            {stop, Error}
    end.

%% @doc account logout
-spec logout(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
logout(State = #client{role_pid = Pid}) ->
    %% notify user server logout
    is_pid(Pid) andalso user_server:cast(Pid, {stop, account_logout}),
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
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, packet_heartbeat_too_fast),
            sender:send(State, Response),
            {stop, normal, State};
        _ ->
            {ok, State#client{heartbeat_time = Now}}
    end.

-dialyzer({no_match, handle_packet/3}).
%% @doc handle packet and packet speed control
-spec handle_packet(State :: #client{}, Protocol :: non_neg_integer(), Data :: [term()]) -> {ok, #client{}} | {stop, term(), #client{}}.
handle_packet(State = #client{role_pid = Pid}, Protocol, Data) ->
    case user_router:interval(State, Protocol) of
        {true, NewState} ->
            %% normal game data
            is_pid(Pid) andalso user_server:socket_event(Pid, Protocol, Data),
            {ok, NewState};
        {false, NewState} ->
            {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, packet_too_fast),
            sender:send(State, Response),
            {ok, NewState}
    end.

%% @doc server version code
-spec version() -> binary().
version() ->
    erlang:apply(version, code, []).
%%%===================================================================
%%% Internal functions
%%%===================================================================
