%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% API
-export([create/10, query/2, login/3, heartbeat/2, move/2, packet_speed/2]).
%% Includes
-include("socket.hrl").
-include("role.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc create account
create(State, AccountName, ServerId, UserName, Sex, Classes, AgentId, Device, Mac, DeviceType) ->
    Sql = io_lib:format("SELECT `id` FROM `role` WHERE `name` = '~s'", [UserName]),
    case word:validate(UserName, [{length, 1, 6}, sensitive, {sql, Sql}]) of
        true ->
            %% failed result reply
            Role = #role{
                account = AccountName,
                name = UserName,
                sex = Sex,
                classes = Classes,
                agent_id = AgentId,
                server_id = ServerId,
                device = Device,
                device_type = DeviceType,
                mac = Mac
            },
            role_sql:insert(Role),
            {ok, Data} = role_route:write(?CMD_ACCOUNT_CREATE, [1]);
        {false, length, _} ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_CREATE, [2]);
        {false, asn1, _} ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_CREATE, [3]);
        {false, sensitive} ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_CREATE, [4]);
        {false, duplicate} ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_CREATE, [5])
    end,
    sender:send(State, Data),
    {ok, State}.

%% @doc query
query(State, AccountName) ->
    case sql:select(io_lib:format("SELECT `name` FROM `role` WHERE `account` = '~s'", [AccountName])) of
        [[Binary]] ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_QUERY, [Binary]);
        _ ->
            {ok, Data} = role_route:write(?CMD_ACCOUNT_QUERY, [<<>>])
    end,
    sender:send(State, Data),
    ok.

%% @doc account login
login(State, ServerId, AccountName) ->
    ThisServerId = config:server_id(),
    %% check account/infant/blacklist etc..
    case sql:select(io_lib:format("SELECT `id` FROM `role` WHERE `account` = '~s'", [AccountName])) of
        [[UserId]] when ServerId == ThisServerId ->
            %% only one match user id
            %% start user process check reconnect first
            check_user_type(UserId, State);
        _ ->
            %% failed result reply
            {ok, Data} = role_route:write(?CMD_ACCOUNT_LOGIN, [0]),
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

%% @doc map position move
move(_, _) ->
    ok.

%% @doc pack speed
packet_speed(State = #client{user_pid = Pid, total_packet_count = TotalCount, total_last_packet_time = LastTime}, _) ->
    Now = time:ts(),
    SpeedTime = 4,
    case TotalCount > 120 andalso LastTime < SpeedTime of
        true ->
            gen_server:cast(Pid, {'heart_error'}),
            {stop, total_packet_count, other_pack_fast, State};
        _ ->
            NewClient = State#client{total_packet_count = 0, total_last_packet_time = Now},
            {ok, NewClient}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
check_user_type(UserId, State) ->
    case catch ets:lookup_element(server_state, server_state, 2) of
        all ->
            check_reconnect(UserId, State);
        Mode ->
            BinaryMode = erlang:atom_to_binary(Mode, utf8),
            case sql:select(io_lib:format("select `type` from `role` where `id` = '~p'", [UserId])) of
                [[BinaryMode]] ->
                    check_reconnect(UserId, State);
                _ ->
                    {ok, Data} = role_route:write(?CMD_ACCOUNT_LOGIN, [0]),
                    sender:send(State, Data),
                    {stop, normal, State}
            end
    end.
%% tpc timeout reconnect
check_reconnect(UserId, State = #client{socket = Socket, socket_type = SocketType, connect_type = ConnectType}) ->
    case process:role_pid(UserId) of
        Pid when is_pid(Pid) ->
            %% replace login
            {ok, Data} = role_route:write(?CMD_ACCOUNT_LOGIN, [1]),
            sender:send(State, Data),
            gen_server:cast(Pid, {'reconnect', self(), Socket, SocketType, ConnectType}),
            {ok, State#client{login_state = login, user_id = UserId, user_pid = Pid}};
        _ ->
            start_login(UserId, State)
    end.
%% common login
start_login(UserId, State = #client{socket = Socket, socket_type = SocketType, connect_type = ConnectType}) ->
    %% new login
    case role_server:start(UserId, self(), Socket, SocketType, ConnectType) of
        {ok, Pid} ->
            %% on select
            gen_server:cast(Pid, 'select'),
            {ok, Data} = role_route:write(?CMD_ACCOUNT_LOGIN, [1]),
            sender:send(State, Data),
            {ok, State#client{login_state = login, user_id = UserId, user_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.
