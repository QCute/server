%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% export API function
-export([create/10, login/2, heartbeat/2, move/2, packet_speed/2]).
%% includes
-include("socket.hrl").
-include("player.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc create account
create(State = #client{socket = Socket, socket_type = SocketType}, AccountName, ServerId, UserName, Sex, Classes, AgentId, Device, Mac, DeviceType) ->
    case sql:select(io_lib:format("SELECT `id` FROM `player` WHERE `name` = '~s'", [UserName])) of
        [] ->
            %% failed result reply
            Player = #player{
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
            player_sql:insert(Player),
            {ok, Data} = player_route:write(?CMD_ACCOUNT_CREATE, [1]),
            SocketType:send(Socket, Data);
        _ ->
            %% only one match user id
            %% start user process check reconnect first
            {ok, Data} = player_route:write(?CMD_ACCOUNT_CREATE, [2]),
            SocketType:send(Socket, Data)
    end,
    {ok, State}.

%% @doc account login
login(State = #client{socket = Socket, socket_type = SocketType}, [_ServerId, UserName]) ->
    %% check account/infant/blacklist etc..
    case sql:select(io_lib:format("SELECT `id` FROM `player` WHERE `name` = '~s'", [UserName])) of
        [[UserId]] ->
            %% only one match user id
            %% start user process check reconnect first
            check_user_type(UserId, State);
        _ ->
            %% failed result reply
            {ok, Data} = player_route:write(?CMD_ACCOUNT_LOGIN, [0]),
            SocketType:send(Socket, Data),
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
check_user_type(UserId, State = #client{socket = Socket, socket_type = SocketType}) ->
    case catch ets:lookup_element(server_state, server_state, 2) of
        all ->
            check_reconnect(UserId, State);
        Mode ->
            BinaryMode = erlang:atom_to_binary(Mode, utf8),
            case sql:select(io_lib:format("select `type` from `player` where `id` = '~p'", [UserId])) of
                [[BinaryMode]] ->
                    check_reconnect(UserId, State);
                _ ->
                    {ok, Data} = player_route:write(?CMD_ACCOUNT_LOGIN, [0]),
                    SocketType:send(Socket, Data),
                    {stop, normal, State}
            end
    end.
%% tpc timeout reconnect
check_reconnect(UserId, State = #client{socket = Socket, socket_type = SocketType}) ->
    case process:player_pid(UserId) of
        Pid when is_pid(Pid) ->
            %% replace login
            gen_server:cast(Pid, {'reconnect', self(), Socket, SocketType}),
            {ok, State#client{login_state = login, user_id = UserId, user_pid = Pid}};
        _ ->
            start_login(UserId, State)
    end.
%% common login
start_login(UserId, State = #client{socket = Socket, socket_type = SocketType}) ->
    %% new login
    case player_server:start(UserId, self(), Socket, SocketType) of
        {ok, Pid} ->
            %% on select
            gen_server:cast(Pid, 'select'),
            {ok, Data} = player_route:write(?CMD_ACCOUNT_LOGIN, [1]),
            SocketType:send(Socket, Data),
            {ok, State#client{login_state = login, user_id = UserId, user_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.
