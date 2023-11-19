%%%-------------------------------------------------------------------
%%% @doc
%%% account control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([heartbeat/1]).
-export([query/3, create/10]).
-export([login/5, logout/1]).
-export([handle_packet/3]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
-include("net.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("device.hrl").
-include("role.hrl").
-include("online.hrl").
-include("event.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc heart beat
-spec heartbeat(Client :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
heartbeat(Client) ->
    case user_router:interval(Client, ?PROTOCOL_ACCOUNT_HEARTBEAT) of
        {true, NewClient} ->
            {ok, NewClient};
        {false, NewClient} ->
            account_handler:send_heartbeat(Client, packet_heartbeat_too_fast),
            {stop, normal, NewClient}
    end.

%% @doc account query
-spec query(Client :: #client{}, ServerId :: non_neg_integer(), Account :: binary()) -> {ok, #client{}}.
query(Client, ServerId, AccountName) ->
    case user_router:interval(Client, ?PROTOCOL_ACCOUNT_QUERY) of
        {true, NewClient} ->
            Result = db:select(<<"SELECT `role_id`, `role_name` FROM `role` WHERE `origin_server_id` = ? AND `account_name` = ?">>, [ServerId, AccountName]),
            List = [list_to_tuple(Row) || Row <- Result],
            account_handler:send_query(Client, ok, List),
            {ok, NewClient};
        {false, NewClient} ->
            account_handler:send_query(Client, packet_too_fast, []),
            {stop, normal, NewClient}
    end.

%% @doc account create
-spec create(Client :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary(), RoleName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), Channel :: binary(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
create(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case create_check_interval(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) of
        {ok, RoleId, NewClient} ->
            account_handler:send_create(Client, ok, RoleId, RoleName),
            {ok, NewClient};
        {error, Reason} ->
            account_handler:send_create(Client, Reason, 0, <<>>),
            {stop, normal, Client}
    end.

create_check_interval(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case user_router:interval(Client, ?PROTOCOL_ACCOUNT_CREATE) of
        {true, NewClient} ->
            create_check_server_id(NewClient, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
        {false, _} ->
            {error, packet_too_fast}
    end.

create_check_server_id(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case ServerId == config:server_id() orelse lists:keymember(ServerId, 1, config:server_id_list()) of
        false ->
            {error, server_id_mismatch};
        true ->
            create_check_state(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_state(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case user_manager:get_create_state() of
        ?FALSE ->
            {error, server_create_forbidden};
        _ ->
            create_check_limit(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_limit(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case db:select_one(<<"SELECT `TABLE_ROWS` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = 'role'">>) >= db:limit() of
        true ->
            {error, account_create_max};
        _ ->
            create_check_sex(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_sex(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case sex_data:get(Sex) of
        <<>> ->
            {error, invalid_sex};
        _ ->
            create_check_classes(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_classes(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    case classes_data:get(Classes) of
        <<>> ->
            {error, invalid_classes};
        _ ->
            create_check_name(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType)
    end.

create_check_name(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
    %% validate name word length and sensitive and duplicate
    case word:validate(RoleName, [{length, 1, 6}, sensitive, {sql, db:format(<<"SELECT `role_name` FROM `role` WHERE `role_name` = ? OR `account_name` = ?">>, [RoleName, AccountName])}]) of
        true ->
            create_start(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);
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

create_start(Client = #client{ip = IP}, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType) ->
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
        channel = Channel,
        register_time = Now
    },
    Device = #device{
        name = DeviceType,
        device_id = DeviceId,
        mac = Mac,
        ip = list_to_binary(inet_parse:ntoa(IP))
    },
    %% name will duplicate
    try
        RoleId = role_sql:insert(Role),
        User = #user{role = Role#role{role_id = RoleId}, device = Device, role_id = RoleId, role_name = RoleName},
        #user{role_id = RoleId} = user_event:trigger(User, #event{name = create}),
        {ok, RoleId, Client}
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {error, internal_error}
    end.

%% @doc account login
-spec login(Client :: #client{}, RoleId :: non_neg_integer(), RoleName :: binary(), ServerId :: non_neg_integer(), AccountName :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(Client, RoleId, RoleName, ServerId, AccountName) ->
    case login_check_interval(Client, RoleId, RoleName, ServerId, AccountName) of
        {ok, NewClient} ->
            account_handler:send_login(Client, ok),
            {ok, NewClient};
        {error, Reason} ->
            account_handler:send_login(Client, Reason),
            {stop, normal, Client}
    end.

login_check_interval(Client, RoleId, RoleName, ServerId, AccountName) ->
    case user_router:interval(Client, ?PROTOCOL_ACCOUNT_LOGIN) of
        {true, NewClient} ->
            login_check_server_id(NewClient, RoleId, RoleName, ServerId, AccountName);
        {false, _} ->
            {error, packet_too_fast}
    end.

login_check_server_id(Client, RoleId, RoleName, ServerId, AccountName) ->
    case ServerId == config:server_id() orelse lists:keymember(ServerId, 1, config:server_id_list()) of
        false ->
            {error, server_id_mismatch};
        true ->
            login_check_server_state(Client, RoleId, RoleName, ServerId, AccountName)
    end.

login_check_server_state(Client, RoleId, RoleName, ServerId, AccountName) ->
    %% server control state
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            {error, server_login_forbidden};
        ?SERVER_STATE_BAN ->
            {error, server_login_forbidden};
        ServerClient ->
            login_check_user(Client, RoleId, RoleName, ServerId, AccountName, ServerClient)
    end.

login_check_user(Client, RoleId, RoleName, ServerId, AccountName, ServerClient) ->
    case db:select(<<"SELECT `type` FROM `role` WHERE `role_id` = ? AND `account_name` = ?">>, [RoleId, AccountName]) of
        [] ->
            %% cannot find role
            {error, account_not_found};
        [[?SERVER_STATE_BAN]] ->
            %% refuse role login
            {error, account_login_forbidden};
        [[Type]] when Type < ServerClient ->
            %% permission denied
            {error, account_permission_denied};
        [[_]] ->
            login_check_started(Client, RoleId, RoleName, ServerId, AccountName)
    end.

login_check_started(Client = #client{role_pid = RolePid}, RoleId, RoleName, ServerId, AccountName) ->
    case is_pid(RolePid) of
        true ->
            {ok, Client};
        false ->
            login_start(Client, RoleId, RoleName, ServerId, AccountName)
    end.

%% common login
login_start(Client = #client{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}, RoleId, RoleName, ServerId, AccountName) ->
    %% new login
    case user_server:start(RoleId, RoleName, ServerId, AccountName, self(), SocketType, Socket, ProtocolType) of
        {ok, Pid} ->
            {ok, Client#client{role_pid = Pid}};
        {error, {already_started, Pid}} ->
            %% reconnect
            gen_server:cast(Pid, {reconnect, self(), SocketType, Socket, ProtocolType}),
            {ok, Client#client{role_pid = Pid}};
        {error, {Reason, Stacktrace}} ->
            ?STACKTRACE(error, Reason, Stacktrace),
            {error, internal_error};
        {error, Reason} ->
            ?PRINT("User Server Start Error: ~0tp", [Reason]),
            {error, internal_error}
    end.

%% @doc account logout
-spec logout(Client :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
logout(Client = #client{role_pid = Pid}) ->
    %% notify user server logout
    user_server:cast(Pid, {stop, normal}),
    account_handler:send_logout(Client, ok),
    %% stop receiver
    {stop, normal, Client}.

%% @doc handle packet and packet speed control
-spec handle_packet(Client :: #client{}, Protocol :: non_neg_integer(), Data :: [term()]) -> {ok, #client{}} | {stop, term(), #client{}}.
handle_packet(Client = #client{role_pid = Pid}, Protocol, Data) ->
    case user_router:interval(Client, Protocol) of
        {true, NewClient} ->
            %% normal game data
            user_server:socket_event(Pid, Protocol, Data),
            {ok, NewClient};
        {false, NewClient} ->
            account_handler:send_logout(Client, packet_too_fast),
            {stop, normal, NewClient}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
