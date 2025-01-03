%%%-------------------------------------------------------------------
%%% @doc
%%% user server
%%% @end
%%%-------------------------------------------------------------------
-module(user_server).
-behaviour(gen_server).
%% API
-export([start/8]).
-export([pid/1, name/1]).
-export([socket_event/3]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4, apply_delay_cast/4, apply_delay_cast/5]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4, pure_delay_cast/4, pure_delay_cast/5]).
-export([call/2, cast/2, info/2]).
-export([field/2, field/3, field/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("time.hrl").
-include("journal.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
%% Macros
-ifdef(DEBUG).
-define(LOGOUT_WAIT_TIME, ?SECOND_MILLISECONDS(3)).
-else.
-define(LOGOUT_WAIT_TIME, ?MINUTE_MILLISECONDS(3)).
-endif.
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(non_neg_integer(), binary(), non_neg_integer(), binary(), pid(), atom(), gen_tcp:socket() | ssl:sslsocket(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, RoleName, ServerId, AccountName, ReceiverPid, SocketType, Socket, ProtocolType) ->
    gen_server:start({local, name(RoleId)}, ?MODULE, [RoleId, RoleName, ServerId, AccountName, ReceiverPid, SocketType, Socket, ProtocolType], []).

%% @doc user server pid
-spec pid(pid()  | non_neg_integer() | atom()) -> pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(RoleId) when is_integer(RoleId) ->
    erlang:whereis(name(RoleId));
pid(Name) when is_atom(Name) ->
    erlang:whereis(Name).

%% @doc user server process register name
-spec name(RoleId :: non_neg_integer()) -> atom().
name(RoleId) ->
    binary_to_atom(<<"role_server_", (integer_to_binary(RoleId))/binary>>, utf8).

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
    apply_call(RoleId, fun(User) -> record:field(User, Field) end, []).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term()) -> term().
field(RoleId, Field, Key) ->
    field(RoleId, Field, Key, 2).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term(), N :: pos_integer()) -> term().
field(RoleId, Field, Key, N) ->
    apply_call(RoleId, fun(User) -> lists:keyfind(Key, N, record:field(User, Field)) end, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #user{}}.
init([RoleId, RoleName, _, _, ReceiverPid, SocketType, Socket, ProtocolType]) ->
    erlang:process_flag(trap_exit, true),
    %% time
    Now = time:now(),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, SocketType, Socket, ProtocolType),
    %% first loop after 3 minutes
    LoopTimer = erlang:start_timer(?MINUTE_MILLISECONDS(3), self(), {loop, 1, Now}),
    %% 30 seconds loops
    User = #user{role_id = RoleId, role_name = RoleName, sender_pid = SenderPid, loop_timer = LoopTimer},
    %% load data
    LoadedUser = event:trigger(User, #event{name = load}),
    %% reset/clean/expire loop
    NewUser = user_loop:loop(LoadedUser, 2, role:logout_time(LoadedUser), Now),
    %% login after loaded
    FinalUser = event:trigger(NewUser, #event{name = login}),
    %% add online user info
    user_manager:add(user_convert:to_online(FinalUser)),
    %% load completed
    {ok, FinalUser}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #user{}) -> {reply, Reply :: term(), NewState :: #user{}}.
handle_call(Request, From, User) ->
    try
        do_call(Request, From, User)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, User}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #user{}) -> {noreply, NewState :: #user{}} | {stop, term(), NewState :: #user{}}.
handle_cast(Request, User) ->
    try
        do_cast(Request, User)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #user{}) -> {noreply, NewState :: #user{}} | {stop, term(), NewState :: #user{}}.
handle_info(Info, User) ->
    try
        do_info(Info, User)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, User}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #user{}) -> {ok, NewState :: #user{}}.
terminate(_Reason, User) ->
    try
        %% save data and logout
        SavedUser = event:trigger(User, #event{name = save}),
        event:trigger(SavedUser, #event{name = logout})
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
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
            user_sender:send(User, NewUser#user.buffer),
            {noreply, NewUser#user{buffer = <<>>}};
        {ok, Reply} ->
            {ok, Binary} = user_router:encode(Protocol, Reply),
            user_sender:send(User, Binary),
            {noreply, User#user{buffer = <<>>}};
        {ok, Reply, NewUser = #user{}} ->
            {ok, Binary} = user_router:encode(Protocol, Reply),
            user_sender:send(User, <<(NewUser#user.buffer)/binary, Binary/binary>>),
            {noreply, NewUser#user{buffer = <<>>}};
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
do_cast({reconnect, ReceiverPid, SocketType, Socket, ProtocolType}, User = #user{role_id = RoleId, loop_timer = LoopTimer}) ->
    %% cancel stop timer
    catch erlang:cancel_timer(LoopTimer),
    %% send duplicate login message
    user_sender:send(User, ?PROTOCOL_ACCOUNT_LOGIN, duplicate),
    %% start sender server
    {ok, SenderPid} = user_sender:start(RoleId, ReceiverPid, SocketType, Socket, ProtocolType),
    %% first loop after 3 minutes
    NewLoopTimer = erlang:start_timer(?MINUTE_MILLISECONDS(3), self(), {loop, 1, time:now()}),
    NewUser = User#user{sender_pid = SenderPid, loop_timer = NewLoopTimer},
    %% reconnect loop
    FinalUser = event:trigger(NewUser, #event{name = reconnect}),
    %% add online user info status(hosting => online)
    user_manager:add(user_convert:to_online(FinalUser)),
    {noreply, FinalUser};
do_cast({disconnect, Reason}, User = #user{loop_timer = LoopTimer}) ->
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% stop sender server
    user_sender:stop(User, Reason),
    %% stop role server after 3 minutes
    NewLoopTimer = erlang:start_timer(?LOGOUT_WAIT_TIME, self(), stop),
    NewUser = User#user{sender_pid = undefined, loop_timer = NewLoopTimer},
    %% save data
    SavedUser = event:trigger(NewUser, #event{name = save}),
    %% disconnect loop
    FinalUser = event:trigger(SavedUser, #event{name = disconnect}),
    %% add online user info status(online => hosting)
    user_manager:add(user_convert:to_hosting(FinalUser)),
    {noreply, FinalUser};
do_cast({stop, Reason}, User = #user{loop_timer = LoopTimer}) ->
    %% cancel loop save data timer
    catch erlang:cancel_timer(LoopTimer),
    %% disconnect and notify client
    user_sender:send(User, ?PROTOCOL_ACCOUNT_LOGOUT, Reason),
    %% stop sender server
    user_sender:stop(User, Reason),
    %% stop after 3 seconds
    NewLoopTimer = erlang:start_timer(?SECOND_MILLISECONDS(3), self(), stop),
    {noreply, User#user{sender_pid = undefined, loop_timer = NewLoopTimer}};
do_cast({send, Protocol, Reply}, User) ->
    user_sender:send(User, Protocol, Reply),
    {noreply, User};
do_cast({send, Binary}, User) ->
    user_sender:send(User, Binary),
    {noreply, User};
do_cast(Request, User) ->
    ?PRINT("Unknown Cast: ~w", [Request]),
    {noreply, User}.


%%%===================================================================
%%% self message call back
%%%===================================================================
do_info({timeout, LoopTimer, stop}, User = #user{role_id = RoleId, loop_timer = LoopTimer}) ->
    %% remove online digest
    user_manager:remove(RoleId),
    %% handle stop
    {stop, normal, User#user{loop_timer = undefined}};
do_info({timeout, LoopTimer, {loop, Tick, Before}}, User = #user{loop_timer = LoopTimer}) ->
    Now = time:now(),
    NewUser = user_loop:loop(User, Tick, Before, Now),
    NextLoopTimer = erlang:start_timer(?SECOND_MILLISECONDS(30), self(), {loop, Tick + 1, Now}),
    {noreply, NewUser#user{loop_timer = NextLoopTimer}};
do_info(Info, User) ->
    ?PRINT("Unknown Info: ~w", [Info]),
    {noreply, User}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
