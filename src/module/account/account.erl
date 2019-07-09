%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([load/1, save/1]).
-export([create/10, query/2, login/3, heartbeat/2, handle_packet/2]).
%% Includes
-include("socket.hrl").
-include("user.hrl").
-include("account.hrl").
-include("role.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API run in user process
%%%===================================================================
%% @doc load 
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Data = account_sql:select(RoleId),
    [Account] = parser:convert(Data, account),
    User#user{account = Account}.

%% @doc save 
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{account = Account}) ->
    account_sql:update(Account),
    User.

%%%===================================================================
%%% API run in receiver process
%%%===================================================================
%% @doc create account
create(State, AccountName, ServerId, UserName, Sex, Classes, AgentId, Device, Mac, DeviceType) ->
    Sql = io_lib:format("SELECT `id` FROM `role` WHERE `name` = '~s'", [UserName]),
    case word:validate(UserName, [{length, 1, 6}, sensitive, {sql, Sql}]) of
        true ->
            Role = #role{
                account_name = AccountName,
                role_name = UserName,
                online = 1,
                sex = Sex,
                classes = Classes,
                server_id = ServerId
            },
            RoleId = role_sql:insert(Role),
            Account = #account{
                role_id = RoleId,
                agent_id = AgentId,
                device = Device,
                device_type = DeviceType,
                mac = Mac
            },
            account_sql:insert(Account),
            {ok, Data} = user_router:write(?CMD_ACCOUNT_CREATE, [1]);
        %% failed result reply
        {false, length, _} ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_CREATE, [2]);
        {false, asn1, _} ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_CREATE, [3]);
        {false, sensitive} ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_CREATE, [4]);
        {false, duplicate} ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_CREATE, [5])
    end,
    sender:send(State, Data),
    {ok, State}.

%% @doc query
query(State, AccountName) ->
    case sql:select(io_lib:format("SELECT `name` FROM `role` WHERE `account_name` = '~s'", [AccountName])) of
        [[Binary]] ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_QUERY, [Binary]);
        _ ->
            {ok, Data} = user_router:write(?CMD_ACCOUNT_QUERY, [<<>>])
    end,
    sender:send(State, Data),
    {ok, State}.

%% @doc account login
login(State, ServerId, AccountName) ->
    ThisServerId = config:server_id(),
    %% check account/infant/blacklist etc..
    case sql:select(io_lib:format("SELECT `id` FROM `role` WHERE `account_name` = '~s'", [AccountName])) of
        [[RoleId]] when ServerId == ThisServerId ->
            %% only one match user id
            %% start user process check reconnect first
            check_user_type(RoleId, State);
        _ ->
            %% failed result reply
            {ok, Data} = user_router:write(?CMD_ACCOUNT_LOGIN, [0]),
            sender:send(State, Data),
            {stop, normal, State}
    end.

%% @doc heart beat
heartbeat(State = #client{user_pid = Pid}, _) ->
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
check_user_type(RoleId, State) ->
    case catch ets:lookup_element(server_state, server_state, 2) of
        all ->
            check_reconnect(RoleId, State);
        Mode ->
            BinaryMode = erlang:atom_to_binary(Mode, utf8),
            case sql:select(io_lib:format("select `type` from `role` where `id` = '~p'", [RoleId])) of
                [[BinaryMode]] ->
                    check_reconnect(RoleId, State);
                _ ->
                    {ok, Data} = user_router:write(?CMD_ACCOUNT_LOGIN, [0]),
                    sender:send(State, Data),
                    {stop, normal, State}
            end
    end.
%% tpc timeout reconnect
check_reconnect(RoleId, State = #client{socket = Socket, socket_type = SocketType, connect_type = ConnectType}) ->
    case process:role_pid(RoleId) of
        Pid when is_pid(Pid) ->
            %% replace login
            {ok, Data} = user_router:write(?CMD_ACCOUNT_LOGIN, [1]),
            sender:send(State, Data),
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
            %% on select
            gen_server:cast(Pid, 'select'),
            {ok, Data} = user_router:write(?CMD_ACCOUNT_LOGIN, [1]),
            sender:send(State, Data),
            {ok, State#client{login_state = login, user_id = RoleId, user_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.
