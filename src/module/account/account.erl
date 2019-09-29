%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([create/10, login/3, heartbeat/1, handle_packet/2]).
%% Includes
-include("socket.hrl").
-include("user.hrl").
-include("online.hrl").
-include("role.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc create account
-spec create(State :: #client{}, AccountName :: binary(), ServerId :: non_neg_integer(), UserName :: binary(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), ChannelId :: non_neg_integer(), DeviceId :: binary(), Mac :: binary(), DeviceType :: binary()) -> {ok, #client{}}.
create(State, AccountName, ServerId, UserName, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType) ->
    Sql = io_lib:format("SELECT `role_id` FROM `role` WHERE `name` = '~s'", [UserName]),
    case word:validate(UserName, [{length, 1, 6}, sensitive, {sql, Sql}]) of
        true ->
            Role = #role{
                account_name = AccountName,
                role_name = UserName,
                online = 1,
                sex = Sex,
                classes = Classes,
                server_id = ServerId,
                channel_id = ChannelId,
                device_id = DeviceId,
                device_type = DeviceType,
                mac = Mac
            },
            role_sql:insert(Role),
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [1]);
        {false, length, _} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [2]);
        {false, asn1, _} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [3]);
        {false, sensitive} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [4]);
        {false, duplicate} ->
            {ok, CreateResponse} = user_router:write(?PROTOCOL_ACCOUNT_CREATE, [5])
    end,
    sender:send(State, CreateResponse),
    {ok, State}.

%% @doc account login
-spec login(State :: #client{}, ServerId :: non_neg_integer(), AccountName :: binary()) -> {ok, #client{}} | {stop, term(), #client{}}.
login(State, ServerId, AccountName) ->
    ThisServerId = config:server_id(),
    %% check account/infant/blacklist etc..
    case sql:select(io_lib:format("SELECT `role_id` FROM `role` WHERE `account_name` = '~s'", [AccountName])) of
        [[RoleId]] when ServerId == ThisServerId ->
            %% only one match user id
            %% start user process check reconnect first
            check_user_type(RoleId, State);
        _ ->
            %% failed result reply
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [2]),
            sender:send(State, LoginResponse),
            {stop, normal, State}
    end.

%% @doc heart beat
-spec heartbeat(State :: #client{}) -> {ok, #client{}} | {stop, term(), #client{}}.
heartbeat(State = #client{user_pid = Pid}) ->
    %% heart packet check
    Now = time:ts(),
    case Now - State#client.heart_last_time < 7 of
        true ->
            gen_server:cast(Pid, {'heart_error'}),
            {stop, heart_pack_fast, State};
        _ ->
            NewState = State#client{heart_last_time = Now, heart_error_count = 0},
            {ok, NewState}
    end.

%% @doc handle packet and packet speed control
-spec handle_packet(State :: #client{}, Data :: [term()]) -> {ok, #client{}} | {stop, term(), #client{}}.
handle_packet(State = #client{login_state = LoginState, protocol = Protocol, user_pid = Pid, total_packet_count = TotalCount, total_last_packet_time = LastTime}, Data) ->
    Now = time:ts(),
    SpeedTime = 4,
    case TotalCount > 120 andalso LastTime < SpeedTime of
        true ->
            gen_server:cast(Pid, {'heart_error'}),
            {stop, total_packet_count, other_pack_fast, State};
        _ ->
            %% common game data
            case LoginState of
                login ->
                    gen_server:cast(Pid, {'socket_event', Protocol, Data});
                _ ->
                    ok
            end,
            NewClient = State#client{total_packet_count = 0, total_last_packet_time = Now},
            {ok, NewClient}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
check_user_type(RoleId, State = #client{server_state = ServerState}) ->
    case ServerState of
        all ->
            check_reconnect(RoleId, State);
        Mode ->
            BinaryMode = erlang:atom_to_binary(Mode, utf8),
            case sql:select(io_lib:format("SELECT `type` FROM `role` WHERE `role_id` = '~p'", [RoleId])) of
                [[BinaryMode]] ->
                    check_reconnect(RoleId, State);
                _ ->
                    {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [3]),
                    sender:send(State, LoginResponse),
                    {stop, normal, State}
            end
    end.
%% tpc timeout reconnect
check_reconnect(RoleId, State = #client{socket = Socket, socket_type = SocketType, connect_type = ConnectType}) ->
    case user_manager:lookup(RoleId) of
        #online{pid = Pid, receiver_pid = ReceiverPid} when is_pid(Pid) ->
            %% replace, send response and stop old receiver
            {ok, DuplicateLoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [4]),
            gen_server:cast(ReceiverPid, {'duplicate_login', DuplicateLoginResponse}),
            %% replace login
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [1]),
            sender:send(State, LoginResponse),
            gen_server:cast(Pid, {'reconnect', self(), Socket, SocketType, ConnectType}),
            {ok, State#client{login_state = login, user_id = RoleId, user_pid = Pid}};
        _ ->
            start_login(RoleId, State)
    end.
%% common login
start_login(RoleId, State = #client{socket = Socket, socket_type = SocketType, connect_type = ConnectType}) ->
    %% new login
    case user_server:start(RoleId, self(), Socket, SocketType, ConnectType) of
        {ok, Pid} ->
            {ok, LoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [1]),
            sender:send(State, LoginResponse),
            {ok, State#client{login_state = login, user_id = RoleId, user_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.
