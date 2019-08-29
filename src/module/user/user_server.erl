%%%-------------------------------------------------------------------
%%% @doc
%%% module role server
%%% @end
%%%-------------------------------------------------------------------
-module(user_server).
-behaviour(gen_server).
%% API
-export([start/5]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4]).
-export([call/2, cast/2, info/2]).
-export([field/2, field/3, field/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
-spec start(non_neg_integer(), pid(), port(), atom(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, SocketType, ConnectType) ->
    gen_server:start({local, process:role_name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, SocketType, ConnectType], []).

%% @doc alert !!! call it debug only
-spec apply_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_call(RoleId, Function, Args) ->
    gen_server:call(process:role_pid(RoleId), {'APPLY_CALL', Function, Args}).

-spec apply_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_call(RoleId, Module, Function, Args) ->
    gen_server:call(process:role_pid(RoleId), {'APPLY_CALL', Module, Function, Args}).

%% @doc main async cast
-spec apply_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(RoleId, Function, Args) ->
    gen_server:cast(process:role_pid(RoleId), {'APPLY_CAST', Function, Args}).

-spec apply_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(RoleId, Module, Function, Args) ->
    gen_server:cast(process:role_pid(RoleId), {'APPLY_CAST', Module, Function, Args}).

%% @doc call (un recommend)
-spec call(pid() | non_neg_integer(), Request :: term()) -> term().
call(RoleId, Request) ->
    gen_server:call(process:role_pid(RoleId), Request).

%% @doc cast
-spec cast(pid() | non_neg_integer(), Request :: term()) -> ok.
cast(RoleId, Request) ->
    gen_server:cast(process:role_pid(RoleId), Request).

%% @doc info
-spec info(pid() | non_neg_integer(), Request :: term()) -> ok.
info(RoleId, Request) ->
    erlang:send(process:role_pid(RoleId), Request).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom()) -> term().
field(RoleId, Field) ->
    apply_call(RoleId, beam, field, [user, Field]).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term()) -> term().
field(RoleId, Field, Key) ->
    field(RoleId, Field, Key, 2).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term(), N :: pos_integer()) -> term().
field(RoleId, Field, Key, N) ->
    lists:keyfind(Key, N, apply_call(RoleId, beam, field, [user, Field])).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([RoleId, ReceiverPid, Socket, SocketType, ConnectType]) ->
    erlang:process_flag(trap_exit, true),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, Socket, SocketType, ConnectType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    %% 30 seconds loop
    User = #user{role_id = RoleId, pid = self(), socket = Socket, receiver_pid = ReceiverPid, socket_type = SocketType, connect_type = ConnectType, sender_pid = SenderPid, timeout = 30 * 1000, loop_timer = LoopTimer},
    NewUser = user_loader:load(User),
    %% add online user info
    user_manager:add(#online{role_id = RoleId, pid = self(), sender_pid = SenderPid, receiver_pid = ReceiverPid, status = online}),
    N = map_server:update_fighter(NewUser),
    {ok, N}.

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
%%-------------------------------------------------------------------
%% main sync role process call back
%%-------------------------------------------------------------------
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
    NewUser = handle_socket_event(User, Protocol, Data),
    {noreply, NewUser};
do_cast({'select'}, User) ->
    %% handle early something on socket select
    {noreply, User};
do_cast({'reconnect', ReceiverPid, Socket, SocketType, ConnectType}, User = #user{role_id = RoleId, logout_timer = LogoutTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LogoutTimer),
    %% start sender server
    {ok, PidSender} = user_sender:start(RoleId, ReceiverPid, Socket, SocketType, ConnectType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    {noreply, User#user{sender_pid = PidSender, receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, loop_timer = LoopTimer}};
do_cast({'disconnect', _Reason}, User = #user{role_id = RoleId, sender_pid = PidSender, loop_timer = LoopTimer}) ->
    %% stop sender server
    user_sender:stop(PidSender),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% stop role server after 5 minutes
    LogoutTimer = erlang:start_timer(1000, self(), 'stop'),
    %% save data
    NewUser = user_saver:save(User),
    %% add online user info status(online => hosting)
    user_manager:add(#online{role_id = RoleId, pid = self(), sender_pid = PidSender, status = hosting}),
    {noreply, NewUser#user{sender_pid = undefined, receiver_pid = undefined, socket = undefined, socket_type = undefined, loop_timer = undefined, logout_timer = LogoutTimer}};
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
    user_sender:send(User, Protocol, Reply),
    {noreply, User};
do_info({'send', Binary}, User = #user{sender_pid = Pid}) ->
    erlang:send(Pid, Binary),
    {noreply, User};
do_info({send_timeout, Id}, User = #user{sender_pid = Pid}) ->
    erlang:send(Pid, {send_timeout, Id}),
    {noreply, User};
do_info({timeout, LogoutTimer, 'stop'}, User = #user{loop_timer = LoopTimer, logout_timer = LogoutTimer}) ->
    %% handle stop
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    {stop, normal, User};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick div 4 == 0 ->
    %% 4 times save important data
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    NewUser = save_timed_first(User),
    {noreply, NewUser#user{tick = Tick + 1, loop_timer = LoopTimer}};
do_info(loop, User = #user{tick = Tick, timeout = Timeout}) when Tick div 6 == 0 ->
    %% 6 times save another secondary data
    LoopTimer = erlang:send_after(Timeout, self(), loop),
    NewUser = save_timed_second(User),
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


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc save data timed
save_timed_first(User) ->
    user_saver:save_loop(#user.account, #user.vip, User).

%% @doc save data timed
save_timed_second(User) ->
    user_saver:save_loop(#user.item, #user.shop, User).

%% handle socket event
handle_socket_event(User, Protocol, Data) ->
    case user_router:handle_routing(User, Protocol, Data) of
        ok ->
            User;
        {ok, NewUser = #user{}} ->
            NewUser;
        {update, NewUser = #user{}} ->
            NewUser;
        {reply, Reply} ->
            user_sender:send(User, Protocol, Reply),
            User;
        {reply, Reply, NewUser = #user{}} ->
            user_sender:send(User, Protocol, Reply),
            NewUser;
        {error, Reply = [_ | _]} ->
            user_sender:send(User, Protocol, Reply),
            User;
        {error, Code} when is_integer(Code) ->
            user_sender:send(User, Protocol, [Code]),
            User;
        {error, protocol, Protocol} ->
            ?DEBUG("~nProtocol: ~p~nData: ~p~n", [Protocol, Data]),
            User;
        _ ->
            User
    end.
