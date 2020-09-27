%%%-------------------------------------------------------------------
%%% @doc
%%% user server
%%% @end
%%%-------------------------------------------------------------------
-module(user_server).
-behaviour(gen_server).
%% API
-export([start/7]).
-export([pid/1, name/1]).
-export([socket_event/3]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4, apply_delay_cast/4, apply_delay_cast/5]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4, pure_delay_cast/4, pure_delay_cast/5]).
-export([call/2, cast/2, info/2]).
-export([field/2, field/3, field/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("online.hrl").
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(non_neg_integer(), binary(), non_neg_integer(), binary(), pid(), port(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, RoleName, ServerId, Account, ReceiverPid, Socket, ProtocolType) ->
    gen_server:start({local, name(RoleId)}, ?MODULE, [RoleId, RoleName, ServerId, Account, ReceiverPid, Socket, ProtocolType], []).

%% @doc user server pid
-spec pid(non_neg_integer() | pid()) -> pid() | undefined.
pid(RoleId) when is_integer(RoleId) ->
    process:pid(name(RoleId));
pid(Pid) when is_pid(Pid) ->
    Pid.

%% @doc user server process register name
-spec name(RoleId :: non_neg_integer()) -> atom().
name(RoleId) ->
    type:to_atom(lists:concat([role_server_, RoleId])).

%% @doc socket event
-spec socket_event(pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: [term()]) -> ok.
socket_event(RoleId, Protocol, Data) ->
    cast(RoleId, {socket_event, Protocol, Data}).

%% @doc pure call, apply f,a with state
-spec apply_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_call(RoleId, Function, Args) ->
    gen_server:call(pid(RoleId), {'APPLY_CALL', Function, Args}).

%% @doc pure call, apply m,f,a with state
-spec apply_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_call(RoleId, Module, Function, Args) ->
    gen_server:call(pid(RoleId), {'APPLY_CALL', Module, Function, Args}).

%% @doc pure call, apply f,a without state
-spec pure_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_call(RoleId, Function, Args) ->
    gen_server:call(pid(RoleId), {'PURE_CALL', Function, Args}).

%% @doc pure call, apply m,f,a without state
-spec pure_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_call(RoleId, Module, Function, Args) ->
    gen_server:call(pid(RoleId), {'PURE_CALL', Module, Function, Args}).

%% @doc apply cast, apply f,a with state
-spec apply_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_cast(RoleId, Function, Args) ->
    gen_server:cast(pid(RoleId), {'APPLY_CAST', Function, Args}).

%% @doc apply cast, apply m,f,a with state
-spec apply_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_cast(RoleId, Module, Function, Args) ->
    gen_server:cast(pid(RoleId), {'APPLY_CAST', Module, Function, Args}).

%% @doc pure cast, apply f,a without state
-spec pure_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_cast(RoleId, Function, Args) ->
    gen_server:cast(pid(RoleId), {'PURE_CAST', Function, Args}).

%% @doc pure cast, apply m,f,a without state
-spec pure_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_cast(RoleId, Module, Function, Args) ->
    gen_server:cast(pid(RoleId), {'PURE_CAST', Module, Function, Args}).

%% @doc main async cast
-spec apply_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
apply_delay_cast(Id, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Function, Args}}).

-spec apply_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
apply_delay_cast(Id, Module, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Module, Function, Args}}).

%% @doc main async cast
-spec pure_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
pure_delay_cast(Id, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Function, Args}}).

-spec pure_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
pure_delay_cast(Id, Module, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Module, Function, Args}}).

%% @doc call
-spec call(pid() | non_neg_integer(), Request :: term()) -> term().
call(RoleId, Request) ->
    gen_server:call(pid(RoleId), Request).

%% @doc cast
-spec cast(pid() | non_neg_integer(), Request :: term()) -> ok.
cast(RoleId, Request) ->
    gen_server:cast(pid(RoleId), Request).

%% @doc info
-spec info(pid() | non_neg_integer(), Request :: term()) -> term().
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #user{}}.
init([RoleId, RoleName, ServerId, Account, ReceiverPid, Socket, ProtocolType]) ->
    erlang:process_flag(trap_exit, true),
    %% time
    Now = time:now(),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, Socket, ProtocolType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_MILLISECONDS(3), self(), {loop, 1, Now}),
    %% 30 seconds loops
    User = #user{role_id = RoleId, role_name = RoleName, server_id = ServerId, account = Account, pid = self(), receiver_pid = ReceiverPid, sender_pid = SenderPid, loop_timer = LoopTimer, login_time = Now},
    %% load data
    LoadedUser = user_loop:load(User),
    %% reset/clean/expire loop
    NewUser = user_loop:loop(LoadedUser, 2, role:online_time(LoadedUser), Now),
    %% login event
    FinalUser = user_event:handle(NewUser, #event{name = login}),
    %% add online user info
    user_manager:add(user_convert:to(FinalUser, online)),
    %% login succeed reply
    user_sender:send(FinalUser, ?PROTOCOL_ACCOUNT_LOGIN, ok),
    %% load completed
    {ok, FinalUser}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #user{}) -> {reply, Reply :: term(), NewState :: #user{}}.
handle_call(Request, From, User) ->
    try
        do_call(Request, From, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, User}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #user{}) -> {noreply, NewState :: #user{}} | {stop, term(), NewState :: #user{}}.
handle_cast(Request, User) ->
    try
        do_cast(Request, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #user{}) -> {noreply, NewState :: #user{}} | {stop, term(), NewState :: #user{}}.
handle_info(Info, User) ->
    try
        do_info(Info, User)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #user{}) -> {ok, NewState :: #user{}}.
terminate(_Reason, User) ->
    try
        %% handle logout event and save data
        user_loop:save(user_event:handle(User, #event{name = logout}))
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {ok, User}
    end.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #user{}, Extra :: term()) -> {ok, NewState :: #user{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% main sync role server call back
%%%===================================================================
do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it only in debug mode.
    case erlang:apply(Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it only in debug mode.
    case erlang:apply(Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it only in debug mode.
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it only in debug mode.
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

%%%===================================================================
%%% main async role server call back
%%%===================================================================
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
    %% socket protocol dispatch
    case user_router:dispatch(User, Protocol, Data) of
        ok ->
            {noreply, User};
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        {ok, Reply} ->
            user_sender:send(User, Protocol, Reply),
            {noreply, User};
        {ok, Reply, NewUser = #user{}} ->
            user_sender:send(NewUser, Protocol, Reply),
            {noreply, NewUser};
        error ->
            {noreply, User};
        {error, Reply} ->
            user_sender:send(User, Protocol, Reply),
            {noreply, User};
        {error, Protocol, Data} ->
            ?PRINT("Unknown Protocol: ~w Data: ~w", [Protocol, Data]),
            {noreply, User};
        What ->
            ?PRINT("Unknown Dispatch Result: ~w", [What]),
            {noreply, User}
    end;
do_cast({reconnect, ReceiverPid, Socket, ProtocolType}, User = #user{role_id = RoleId, receiver_pid = OldReceiverPid, logout_timer = LogoutTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LogoutTimer),
    %% replace, send response and stop old receiver
    {ok, DuplicateLoginResponse} = user_router:write(?PROTOCOL_ACCOUNT_LOGIN, duplicate),
    gen_server:cast(OldReceiverPid, {stop, DuplicateLoginResponse}),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, Socket, ProtocolType),
    %% first loop after 3 minutes
    LoopTimer = erlang:send_after(?MINUTE_MILLISECONDS(3), self(), loop),
    %% enter map
    NewUser = User#user{sender_pid = SenderPid, receiver_pid = ReceiverPid, loop_timer = LoopTimer, logout_timer = undefined},
    %% handle reconnect event
    FinalUser = user_event:handle(NewUser, #event{name = reconnect}),
    %% add online user info status(online => hosting)
    user_manager:add(user_convert:to(NewUser, online)),
    %% reconnect success reply
    user_sender:send(FinalUser, ?PROTOCOL_ACCOUNT_LOGIN, ok),
    {noreply, FinalUser};
do_cast({disconnect, _Reason}, User = #user{sender_pid = SenderPid, loop_timer = LoopTimer}) ->
    %% stop sender server
    catch gen_server:stop(SenderPid),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% stop role server after 3 minutes
    LogoutTimer = erlang:start_timer(?MINUTE_MILLISECONDS(3), self(), stop),
    NewUser = User#user{sender_pid = undefined, receiver_pid = undefined, loop_timer = undefined, logout_timer = LogoutTimer},
    %% save data
    SavedUser = user_loop:save(NewUser),
    %% handle disconnect event
    FinalUser = user_event:handle(SavedUser, #event{name = disconnect}),
    %% add online user info status(online => hosting)
    user_manager:add(user_convert:to(NewUser, hosting)),
    {noreply, FinalUser};
do_cast({stop, Reason}, User = #user{role_id = RoleId, loop_timer = LoopTimer, sender_pid = SenderPid, receiver_pid = ReceiverPid}) ->
    %% remove online digest
    user_manager:remove(RoleId),
    %% leave map
    NewUser = map_server:leave(User),
    %% stop sender server
    catch gen_server:stop(SenderPid),
    %% disconnect and notify client
    {ok, Response} = user_router:write(?PROTOCOL_ACCOUNT_LOGOUT, Reason),
    gen_server:cast(ReceiverPid, {stop, Response}),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% handle stop
    {stop, normal, NewUser};
do_cast({send, Protocol, Reply}, User) ->
    user_sender:send(User, Protocol, Reply),
    {noreply, User};
do_cast({send, Binary}, User) ->
    user_sender:send(User, Binary),
    {noreply, User};
do_cast(_Request, User) ->
    {noreply, User}.

%%%===================================================================
%%% self message call back
%%%===================================================================
do_info({timeout, LogoutTimer, stop}, User = #user{role_id = RoleId, loop_timer = LoopTimer, logout_timer = LogoutTimer}) ->
    %% remove online digest
    user_manager:remove(RoleId),
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    {stop, normal, User};
do_info({loop, Tick, Before}, User) ->
    Now = time:now(),
    NewUser = user_loop:loop(User, Tick, Before, Now),
    LoopTimer = erlang:send_after(?MILLISECONDS(30), self(), {loop, Tick + 1, Now}),
    {noreply, NewUser#user{loop_timer = LoopTimer}};
do_info(_Info, User) ->
    {noreply, User}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
