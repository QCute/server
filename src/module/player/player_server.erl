%%%-------------------------------------------------------------------
%%% @doc
%%% module player server
%%% @end
%%%-------------------------------------------------------------------
-module(player_server).
-behaviour(gen_server).
%% API
-export([start/4]).
-export([call/2, call/3, call/4, cast/2, cast/3, cast/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("player.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(UserId, ReceiverPid, Socket, SocketType) ->
    gen_server:start(?MODULE, [UserId, ReceiverPid, Socket, SocketType], []).

%% @doc alert !!! call it debug only
call(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function}).
call(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function, Args}).
call(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Module, Function, Args}).

%% @doc main async cast
cast(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function}).
cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function, Args}).
cast(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Module, Function, Args}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserId, ReceiverPid, Socket, SocketType]) ->
    erlang:process_flag(trap_exit, true),
    erlang:register(process:player_name(UserId), self()),
    %% start sender server
    {ok, PidSender} = player_sender:start(UserId, ReceiverPid, Socket, SocketType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    %% 30 seconds loop
    User = #user{id = UserId, pid = self(), socket = Socket, pid_receiver = ReceiverPid, socket_type = SocketType, pid_sender = PidSender, timeout = 30 * 1000, loop_timer = LoopTimer},
    NewUser = player_login:login(User),
    %% add online user info
    player_manager:add(#online{id = UserId, pid = self(), pid_sender = PidSender, status = online}),
    {ok, NewUser}.

handle_call(Request, From, User) ->
    try
        do_call(Request, From, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, User}
    end.

handle_cast(Request, User) ->
    try
        do_cast(Request, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

handle_info(Info, User) ->
    try
        do_info(Info, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

terminate(_Reason, User = #user{id = Id}) ->
    try
        player_logout:logout(User),
        player_manager:remove(Id)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%-------------------------------------------------------------------
%% main sync player process call back
%%-------------------------------------------------------------------
do_call({'APPLY_CALL', Function}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Function, [User]), User};
do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Function, [User | Args]), User};
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Module, Function, [User | Args]), User};
do_call({'SOCKET_EVENT', Protocol, Data}, _From, User) ->
    %% alert !!! call it debug only
    NewUser = socket_event(User, Protocol, Data),
    {reply, ok, NewUser};
do_call(_Request, _From, User) ->
    {reply, ok, User}.

%%-------------------------------------------------------------------
%% main async player process call back
%%-------------------------------------------------------------------
do_cast({'APPLY_CAST', Function}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Function, [User]),
    {noreply, User};
do_cast({'APPLY_CAST', Function, Args}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Function, [User | Args]),
    {noreply, User};
do_cast({'APPLY_CAST', Module, Function, Args}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Module, Function, [User | Args]),
    {noreply, User};
do_cast({'SOCKET_EVENT', Protocol, Data}, User) ->
    %% socket protocol
    NewUser = socket_event(User, Protocol, Data),
    {noreply, NewUser};
do_cast({'select'}, User) ->
    %% handle early something on socket select
    {noreply, User};
do_cast({'reconnect', ReceiverPid, Socket, SocketType}, User = #user{id = UserId, logout_timer = LogoutTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LogoutTimer),
    %% start sender server
    {ok, PidSender} = player_sender:start(UserId, ReceiverPid, Socket, SocketType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    {noreply, User#user{pid_sender = PidSender, pid_receiver = ReceiverPid, socket = Socket, socket_type = SocketType, loop_timer = LoopTimer}};
do_cast({'disconnect', _Reason}, User = #user{id = UserId, pid_sender = PidSender, loop_timer = LoopTimer}) ->
    %% stop sender server
    player_sender:stop(PidSender),
	%% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
	%% stop player server after 5 minutes
    LogoutTimer = erlang:start_timer(?MINUTE_SECONDS * 5 * 1000, self(), 'stop'),
	%% save data
    NewUser = player_logout:logout(User),
    %% add online user info status(online => hosting)
    player_manager:add(#online{id = UserId, pid = self(), pid_sender = PidSender, status = hosting}),
    {noreply, NewUser#user{pid_sender = undefined, pid_receiver = undefined, socket = undefined, socket_type = undefined, loop_timer = undefined, logout_timer = LogoutTimer}};
do_cast('stop', User = #user{loop_timer = LoopTimer}) ->
    %% disconnect client
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% handle stop
    {stop, normal, User};
do_cast(_Request, User) ->
    {noreply, User}.

%% un recommend
do_info({'send', Protocol, Reply}, User) ->
    reply(User, Protocol, Reply),
    {noreply, User};
do_info({'send', Binary}, User = #user{pid_sender = Pid}) ->
    erlang:send(Pid, Binary),
    {noreply, User};
do_info({timeout, LogoutTimer, 'stop'}, User = #user{loop_timer = LoopTimer, logout_timer = LogoutTimer}) ->
    %% handle stop
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    {stop, normal, User};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick div 4 == 0 ->
    %% 统一定时处理
    erlang:send_after(Timeout, self(), loop),
    NewUser = player:save_timed_first(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick div 6 == 0 ->
    %% 统一定时处理
    erlang:send_after(Timeout, self(), loop),
    NewUser = player:save_timed_second(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) ->
    %% 统一定时处理
    erlang:send_after(Timeout, self(), loop),
    NewUser = player:save_timed_second(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(_Info, User) ->
    {noreply, User}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% handle socket event
socket_event(User, Protocol, Data) ->
    %% Result = do_routing(User, Protocol, Data),
    case player_route:handle_routing(User, Protocol, Data) of
        ok ->
            User;
        {ok, NewUser = #user{}} ->
            NewUser;
        {update, NewUser = #user{}} ->
            NewUser;
        {reply, Reply} ->
            reply(User, Protocol, Reply),
            User;
        {relpy, Reply, NewUser = #user{}} ->
            reply(User, Protocol, Reply),
            NewUser;
        {error, unknow_command} ->
            User;
        _ ->
            User
    end.

%% push reply to client
reply(#user{pid_sender = Pid}, Protocol, Reply) ->
    reply(Pid, Protocol, Reply);
reply(Pid, Protocol, Reply) when is_pid(Pid) ->
    case player_route:write(Protocol, Reply) of
        {ok, Data} ->
            erlang:send(Pid, {'SEND', Data});
        _ ->
            {error, pack_data_error}
    end.