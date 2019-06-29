%%%-------------------------------------------------------------------
%%% @doc
%%% module role server
%%% @end
%%%-------------------------------------------------------------------
-module(role_server).
-behaviour(gen_server).
%% API
-export([start/5]).
-export([apply_call/2, apply_call/3, apply_call/4, apply_cast/2, apply_cast/3, apply_cast/4]).
-export([call/2, cast/2, info/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("online.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(UserId, ReceiverPid, Socket, SocketType, ConnectType) ->
    gen_server:start(?MODULE, [UserId, ReceiverPid, Socket, SocketType, ConnectType], []).

%% @doc alert !!! call it debug only
apply_call(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function}).
apply_call(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function, Args}).
apply_call(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Module, Function, Args}).

%% @doc main async cast
apply_cast(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function}).
apply_cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function, Args}).
apply_cast(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Module, Function, Args}).

%% @doc call (un recommend)
-spec call(pid() | non_neg_integer(), Request :: term()) -> term().
call(Id, Request) when is_integer(Id) ->
    call(process:role_pid(Id), Request);
call(Pid, Request) when is_pid(Pid) ->
    gen_server:call(Pid, Request).

%% @doc cast
-spec cast(pid() | non_neg_integer(), Request :: term()) -> ok.
cast(Id, Request) when is_integer(Id) ->
    cast(process:role_pid(Id), Request);
cast(Pid, Request) when is_pid(Pid) ->
    gen_server:cast(Pid, Request).

%% @doc info
-spec info(pid() | non_neg_integer(), Request :: term()) -> ok.
info(Id, Request) when is_integer(Id) ->
    info(process:role_pid(Id), Request);
info(Pid, Request) when is_pid(Pid) ->
    erlang:send(Pid, Request).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserId, ReceiverPid, Socket, SocketType, ConnectType]) ->
    erlang:process_flag(trap_exit, true),
    erlang:register(process:role_name(UserId), self()),
    %% start sender server
    {ok, PidSender} = role_sender:start(UserId, ReceiverPid, Socket, SocketType, ConnectType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    %% 30 seconds loop
    User = #user{id = UserId, pid = self(), socket = Socket, pid_receiver = ReceiverPid, socket_type = SocketType, connect_type = ConnectType, pid_sender = PidSender, timeout = 30 * 1000, loop_timer = LoopTimer},
    NewUser = role_login:login(User),
    %% add online user info
    role_manager:add(#online{id = UserId, pid = self(), pid_sender = PidSender, status = online}),
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
        role_logout:logout(User),
        role_manager:remove(Id)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%-------------------------------------------------------------------
%% main sync role process call back
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
do_call(_Request, _From, User) ->
    {reply, ok, User}.

%%-------------------------------------------------------------------
%% main async role process call back
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
do_cast({'socket_event', Protocol, Data}, User) ->
    %% socket protocol
    NewUser = socket_event(User, Protocol, Data),
    {noreply, NewUser};
do_cast({'select'}, User) ->
    %% handle early something on socket select
    {noreply, User};
do_cast({'reconnect', ReceiverPid, Socket, SocketType, ConnectType}, User = #user{id = UserId, logout_timer = LogoutTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LogoutTimer),
    %% start sender server
    {ok, PidSender} = role_sender:start(UserId, ReceiverPid, Socket, SocketType, ConnectType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    {noreply, User#user{pid_sender = PidSender, pid_receiver = ReceiverPid, socket = Socket, socket_type = SocketType, loop_timer = LoopTimer}};
do_cast({'disconnect', _Reason}, User = #user{id = UserId, pid_sender = PidSender, loop_timer = LoopTimer}) ->
    %% stop sender server
    role_sender:stop(PidSender),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% stop role server after 5 minutes
    LogoutTimer = erlang:start_timer(1000, self(), 'stop'),
    %% save data
    NewUser = role_logout:logout(User),
    %% add online user info status(online => hosting)
    role_manager:add(#online{id = UserId, pid = self(), pid_sender = PidSender, status = hosting}),
    {noreply, NewUser#user{pid_sender = undefined, pid_receiver = undefined, socket = undefined, socket_type = undefined, loop_timer = undefined, logout_timer = LogoutTimer}};
do_cast({'stop', server_update}, User = #user{loop_timer = LoopTimer}) ->
    %% disconnect client
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% handle stop
    {stop, normal, User};
do_cast(_Request, User) ->
    {noreply, User}.

%%-------------------------------------------------------------------
%% self message call back
%%-------------------------------------------------------------------
%% un recommend
do_info({'send', Protocol, Reply}, User) ->
    role_sender:send(User, Protocol, Reply),
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
    %% 4 times save important data
    erlang:send_after(Timeout, self(), loop),
    NewUser = role:save_timed_first(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick div 6 == 0 ->
    %% 6 times save another secondary data
    erlang:send_after(Timeout, self(), loop),
    NewUser = role:save_timed_second(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) ->
    %% other times do something etc...
    erlang:send_after(Timeout, self(), loop),
    NewUser = role:save_timed_second(User),
    {noreply, NewUser#user{tick = Tick + 1}};
do_info(_Info, User) ->
    {noreply, User}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% handle socket event
socket_event(User, Protocol, Data) ->
    case role_route:handle_routing(User, Protocol, Data) of
        ok ->
            User;
        {ok, NewUser = #user{}} ->
            NewUser;
        {update, NewUser = #user{}} ->
            NewUser;
        {reply, Reply} ->
            role_sender:send(User, Protocol, Reply),
            User;
        {reply, Reply, NewUser = #user{}} ->
            role_sender:send(User, Protocol, Reply),
            NewUser;
        {error, unknown_command} ->
            User;
        {error, protocol, Protocol} ->
            ?DEBUG("~nProtocol: ~p~nData: ~p~n", [Protocol, Data]);
        _ ->
            User
    end.