%%%-------------------------------------------------------------------
%%% @doc
%%% module account/user control
%%% @end
%%%-------------------------------------------------------------------
-module(account).
%% export API function
-export([create/2, login/2, heart_beat/2, move/2, packet_speed/2]).
%% includes
-include("socket.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc create account
create(State, []) ->
    {ok, State}.

%% @doc account login
login(State = #client{socket = Socket, socket_type = SocketType}, [_ServerId, UserName]) ->
    %% todo check account/infant/blacklist etc..
    case sql:select(io_lib:format("SELECT `id` FROM `player` WHERE `name` = '~s'", [UserName])) of
        [[UserId]] ->
            %% only one match user id
            %% start user process check reconnect first
            check_reconnect(UserId, State);
        _ ->
            %% failed result reply
            {ok, Data} = player_route:write(?PP_ACCOUNT_LOGIN, [0]),
            SocketType:send(Socket, Data),
            {ok, State}
    end.

%% @doc heart beat
heart_beat(State = #client{user_pid = Pid}, _) ->
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
            {ok, Data} = player_route:write(?PP_ACCOUNT_LOGIN, [1]),
            SocketType:send(Socket, Data),
            {ok, State#client{login_state = login, user_id = UserId, user_pid = Pid}};
        Error ->
            {stop, Error, State}
    end.
