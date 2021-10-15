%%%-------------------------------------------------------------------
%%% @doc
%%% user sender
%%% @end
%%%-------------------------------------------------------------------
-module(user_sender).
-behaviour(gen_server).
-compile({no_auto_import, [send/2, send/3]}).
-compile({nowarn_unused_function, start_receiver/4}).
-compile({nowarn_unused_function, start_sender/4}).
-compile({nowarn_unused_function, stop_receiver/2}).
-compile({nowarn_unused_function, stop_sender/2}).
%% API
-export([start/4, stop/1, stop/2]).
-export([pid/1, name/1]).
-export([send/2, send/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("user.hrl").
%% user sender state
-record(state, {role_id, receiver_pid, socket, protocol_type}).
%% Macros
-ifndef(SENDER).
-define(START(RoleId, ReceiverPid, Socket, ProtocolType), start_receiver(RoleId, ReceiverPid, Socket, ProtocolType)).
-define(STOP(User, Reason), stop_receiver(User, Reason)).
-else.
-define(START(RoleId, ReceiverPid, Socket, ProtocolType), start_sender(RoleId, ReceiverPid, Socket, ProtocolType)).
-define(STOP(User, Reason), stop_sender(User, Reason)).
-endif.
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(RoleId :: non_neg_integer(), ReceiverPid :: pid(), Socket :: port(), ProtocolType :: atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, ProtocolType) ->
    ?START(RoleId, ReceiverPid, Socket, ProtocolType).

%% use receiver as sender
start_receiver(RoleId, ReceiverPid, _, _) ->
    Name = name(RoleId),
    case erlang:whereis(Name) of
        undefined ->
            ok;
        Pid ->
            try
                %% stop old receiver
                gen_server:stop(Pid, normal, ?SECOND_MILLISECONDS(3))
            catch ?EXCEPTION(Class, Reason, Stacktrace) ->
                ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
            end
    end,
    erlang:register(Name, ReceiverPid),
    {ok, ReceiverPid}.

%% use alone process as sender
start_sender(RoleId, ReceiverPid, Socket, ProtocolType) ->
    case gen_server:start_link({local, name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, ProtocolType], []) of
        {error, {already_started, Pid}} ->
            %% replace socket
            gen_server:cast(Pid, {reconnect, ReceiverPid, Socket, ProtocolType}),
            {ok, Pid};
        Result ->
            Result
    end.

%% @doc stop
-spec stop(#user{}) -> ok.
stop(User) ->
    stop(User, normal).

%% @doc stop
-spec stop(#user{}, Reason :: term()) -> ok.
stop(User, Reason) ->
    ?STOP(User, Reason).

%% stop receiver process
stop_receiver(#user{sender_pid = undefined}, _) ->
    ok;
stop_receiver(#user{sender_pid = Pid}, normal) ->
    try
        %% stop receiver
        gen_server:stop(Pid, normal, ?SECOND_MILLISECONDS(3))
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end;
stop_receiver(_, _) ->
    ok.

%% stop alone sender process
stop_sender(#user{sender_pid = undefined}, _) ->
    ok;
stop_sender(#user{sender_pid = SenderPid}, Reason) ->
    gen_server:stop(SenderPid, Reason, ?CALL_TIMEOUT).

%% @doc user sender pid
-spec pid(pid()  | non_neg_integer() | atom()) -> pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(RoleId) when is_integer(RoleId) ->
    erlang:whereis(name(RoleId));
pid(Name) when is_atom(Name) ->
    erlang:whereis(Name).

%% @doc user sender process register name
-spec name(RoleId :: non_neg_integer()) -> atom().
name(RoleId) ->
    binary_to_atom(<<"role_sender_", (integer_to_binary(RoleId))/binary>>, utf8).

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: term()) -> ok.
send(#user{sender_pid = Pid}, Protocol, Data) ->
    {ok, Binary} = user_router:write(Protocol, Data),
    send(Pid, Binary);
send(RoleId, Protocol, Data) ->
    {ok, Binary} = user_router:write(Protocol, Data),
    send(pid(RoleId), Binary).

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Binary :: binary()) -> ok.
send(_, <<>>) ->
    ok;
send(#user{sender_pid = Pid}, Binary) ->
    gen_server:cast(Pid, {send, Binary});
send(RoleId, Binary) ->
    gen_server:cast(pid(RoleId), {send, Binary}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([RoleId, ReceiverPid, Socket, ProtocolType]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{role_id = RoleId, receiver_pid = ReceiverPid, socket = Socket, protocol_type = ProtocolType}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, Reply :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast({send, Binary}, State = #state{socket = Socket, protocol_type = ProtocolType}) ->
    try
        %% send binary packet
        sender:send(Socket, ProtocolType, Binary)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, State};

handle_cast({reconnect, ReceiverPid, Socket, ProtocolType}, State) ->
    try
        %% stop old receiver
        gen_server:stop(State#state.receiver_pid, normal, ?SECOND_MILLISECONDS(3))
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    %% replace new receiver
    {noreply, State#state{receiver_pid = ReceiverPid, socket = Socket, protocol_type = ProtocolType}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info({sender, ok}, State) ->
    %% sender ssl:send/2 => $gen_call
    {noreply, State};
handle_info(Info, State) ->
    ?PRINT("Unknown User Sender Message:~w~n", [Info]),
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> {ok, NewState :: #state{}}.
terminate(normal, State) ->
    try
        %% stop receiver
        gen_server:stop(State#state.receiver_pid, normal, ?SECOND_MILLISECONDS(3))
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State};
terminate(_, State) ->
    %% receiver closed
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
