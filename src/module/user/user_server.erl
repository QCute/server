%%%------------------------------------------------------------------
%%% @doc
%%% module role server
%%% @end
%%%------------------------------------------------------------------
-module(user_server).
-behaviour(gen_server).
%% API
-export([start/5]).
-export([pid/1, name/1]).
-export([socket_event/3]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4]).
-export([call/2, cast/2, info/2]).
-export([field/2, field/3, field/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start(non_neg_integer(), pid(), port(), atom(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, SocketType, ProtocolType) ->
    gen_server:start({local, name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, SocketType, ProtocolType], []).

%% @doc 获取角色进程Pid
-spec pid(non_neg_integer() | pid()) -> Pid :: pid() | undefined.
pid(RoleId) when is_integer(RoleId) ->
    process:where(name(RoleId));
pid(Pid) when is_pid(Pid) ->
    Pid.

%% @doc 角色进程名
-spec name(RoleId :: non_neg_integer()) -> atom().
name(RoleId) ->
    type:to_atom(lists:concat([role_server_, RoleId])).

%% @doc socket event
-spec socket_event(pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: [term()]) -> ok.
socket_event(RoleId, Protocol, Data) ->
    gen_server:cast(pid(RoleId), {socket_event, Protocol, Data}).

%% @doc pure call, apply f,a with state
-spec apply_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_call(RoleId, Function, Args) ->
    gen_server:call(pid(RoleId), {'APPLY_CALL', Function, Args}).

%% @doc pure call, apply m,f,a with state
-spec apply_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_call(RoleId, Module, Function, Args) ->
    gen_server:call(pid(RoleId), {'APPLY_CALL', Module, Function, Args}).

%% @doc pure call, apply f,a without state
-spec pure_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
pure_call(RoleId, Function, Args) ->
    gen_server:call(pid(RoleId), {'PURE_CALL', Function, Args}).

%% @doc pure call, apply m,f,a without state
-spec pure_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_call(RoleId, Module, Function, Args) ->
    gen_server:call(pid(RoleId), {'PURE_CALL', Module, Function, Args}).

%% @doc apply cast, apply f,a with state
-spec apply_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(RoleId, Function, Args) ->
    gen_server:cast(pid(RoleId), {'APPLY_CAST', Function, Args}).

%% @doc apply cast, apply m,f,a with state
-spec apply_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(RoleId, Module, Function, Args) ->
    gen_server:cast(pid(RoleId), {'APPLY_CAST', Module, Function, Args}).

%% @doc pure cast, apply f,a without state
-spec pure_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
pure_cast(RoleId, Function, Args) ->
    gen_server:cast(pid(RoleId), {'PURE_CAST', Function, Args}).

%% @doc pure cast, apply m,f,a without state
-spec pure_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_cast(RoleId, Module, Function, Args) ->
    gen_server:cast(pid(RoleId), {'PURE_CAST', Module, Function, Args}).

%% @doc call
-spec call(pid() | non_neg_integer(), Request :: term()) -> term().
call(RoleId, Request) ->
    gen_server:call(pid(RoleId), Request).

%% @doc cast
-spec cast(pid() | non_neg_integer(), Request :: term()) -> ok.
cast(RoleId, Request) ->
    gen_server:cast(pid(RoleId), Request).

%% @doc info
-spec info(pid() | non_neg_integer(), Request :: term()) -> ok.
info(RoleId, Request) ->
    erlang:send(pid(RoleId), Request).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom()) -> term().
field(RoleId, Field) ->
    apply_call(RoleId, fun(User) -> beam:field(User, Field) end, []).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term()) -> term().
field(RoleId, Field, Key) ->
    field(RoleId, Field, Key, 2).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term(), N :: pos_integer()) -> term().
field(RoleId, Field, Key, N) ->
    apply_call(RoleId, fun(User) -> lists:keyfind(Key, N, beam:field(User, Field)) end, []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([RoleId, ReceiverPid, Socket, SocketType, ProtocolType]) ->
    erlang:process_flag(trap_exit, true),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, Socket, SocketType, ProtocolType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    %% 30 seconds loop
    User = #user{role_id = RoleId, pid = self(), socket = Socket, receiver_pid = ReceiverPid, socket_type = SocketType, connect_type = ProtocolType, sender_pid = SenderPid, timeout = 30 * 1000, loop_timer = LoopTimer},
    NewUser = user_loader:load(User),
    %% add online user info
    user_manager:add(#online{role_id = RoleId, pid = self(), sender_pid = SenderPid, receiver_pid = ReceiverPid, status = online}),
    %% enter map
    %% FinalUser = map_server:enter(NewUser),
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

terminate(_Reason, User = #user{role_id = RoleId}) ->
    try
        user_saver:save(User),
        user_manager:remove(RoleId)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% main sync role process call back
%%%==================================================================
do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call(_Request, _From, User) ->
    {reply, ok, User}.

%%%==================================================================
%%% main async role process call back
%%%==================================================================
do_cast({'APPLY_CAST', Function, Args}, User) ->
    case erlang:apply(Function, [User | Args]) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'PURE_CAST', Function, Args}, User) ->
    case erlang:apply(Function, Args) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'APPLY_CAST', Module, Function, Args}, User) ->
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'PURE_CAST', Module, Function, Args}, User) ->
    case erlang:apply(Module, Function, Args) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({socket_event, Protocol, Data}, User) ->
    %% socket protocol
    NewUser = handle_socket_event(User, Protocol, Data),
    {noreply, NewUser};
do_cast({reconnect, ReceiverPid, Socket, SocketType, ProtocolType}, User = #user{role_id = RoleId, receiver_pid = OldReceiverPid, logout_timer = LogoutTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LogoutTimer),
    %% replace, send response and stop old receiver
    {ok, DuplicateLoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, [5]),
    gen_server:cast(OldReceiverPid, {stop, DuplicateLoginResponse}),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, Socket, SocketType, ProtocolType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    {noreply, User#user{sender_pid = SenderPid, receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, loop_timer = LoopTimer}};
do_cast({disconnect, _Reason}, User = #user{role_id = RoleId, sender_pid = SenderPid, loop_timer = LoopTimer}) ->
    %% stop sender server
    user_sender:stop(SenderPid),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% stop role server after 5 minutes
    LogoutTimer = erlang:start_timer(?MINUTE_MILLISECONDS(5), self(), stop),
    %% save data
    NewUser = user_saver:save(User),
    %% add online user info status(online => hosting)
    user_manager:add(#online{role_id = RoleId, pid = self(), sender_pid = undefined, receiver_pid = undefined, status = hosting}),
    {noreply, NewUser#user{sender_pid = undefined, receiver_pid = undefined, socket = undefined, socket_type = undefined, loop_timer = undefined, logout_timer = LogoutTimer}};
do_cast({stop, server_update}, User = #user{loop_timer = LoopTimer}) ->
    %% disconnect client
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% handle stop
    {stop, normal, User};
do_cast({packet_fast_error, _Reason}, User = #user{sender_pid = SenderPid, loop_timer = LoopTimer}) ->
    %% disconnect client
    user_sender:stop(SenderPid),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% handle stop
    {stop, normal, User};
do_cast({send, Protocol, Reply}, User) ->
    user_sender:send(User, Protocol, Reply),
    {noreply, User};
do_cast({send, Binary}, User = #user{sender_pid = Pid}) ->
    erlang:send(Pid, Binary),
    {noreply, User};
do_cast(_Request, User) ->
    {noreply, User}.

%%%==================================================================
%%% self message call back
%%%==================================================================
do_info({timeout, LogoutTimer, stop}, User = #user{loop_timer = LoopTimer, logout_timer = LogoutTimer}) ->
    %% handle stop
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    {stop, normal, User};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick rem 4 == 0 ->
    %% 4 times save important data
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    NewUser = user_saver:save_loop(#user.role, #user.vip, User),
    {noreply, NewUser#user{tick = Tick + 1, loop_timer = LoopTimer}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick rem 6 == 0 ->
    %% 6 times save another secondary data
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    NewUser = user_saver:save_loop(#user.item, #user.shop, User),
    {noreply, NewUser#user{tick = Tick + 1, loop_timer = LoopTimer}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick rem 8 == 0 ->
    %% 6 times save another secondary data
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    NewUser = user_saver:save_loop(#user.buff, #user.count, User),
    {noreply, NewUser#user{tick = Tick + 1, loop_timer = LoopTimer}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) ->
    %% other times do something etc...
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    Now = time:ts(),
    case time:cross(day, 0, Now - Timeout, Now) of
        true ->
            NewUser = user_cleaner:clean(User);
        false ->
            NewUser = User
    end,
    {noreply, NewUser#user{tick = Tick + 1, loop_timer = LoopTimer}};
do_info(_Info, User) ->
    {noreply, User}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% @doc handle socket event
handle_socket_event(User, Protocol, Data) ->
    case user_router:dispatch(User, Protocol, Data) of
        {ok, NewUser = #user{}} ->
            NewUser;
        {ok, Reply = [_ | _]} ->
            user_sender:send(User, Protocol, Reply),
            User;
        {ok, Code} when is_integer(Code) ->
            user_sender:send(User, Protocol, [Code]),
            User;
        {ok, Reply = [_ | _], NewUser = #user{}} ->
            user_sender:send(User, Protocol, Reply),
            NewUser;
        {ok, Code, NewUser = #user{}} when is_integer(Code) ->
            user_sender:send(User, Protocol, [Code]),
            NewUser;
        {error, Reply = [_ | _]} ->
            user_sender:send(User, Protocol, Reply),
            User;
        {error, Code} when is_integer(Code) ->
            user_sender:send(User, Protocol, [Code]),
            User;
        {error, protocol, Protocol} ->
            ?PRINT("Protocol: ~w Data: ~w", [Protocol, Data]),
            User;
        _ ->
            User
    end.
