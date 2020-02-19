%%%------------------------------------------------------------------
%%% @doc
%%% module role sender
%%% @end
%%%------------------------------------------------------------------
-module(user_sender).
-behaviour(gen_server).
-compile({no_auto_import, [send/2, send/3]}).
%% API
-export([start/5]).
-export([pid/1, name/1]).
-export([send/2, send/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("user.hrl").
%% user sender state
-record(state, {role_id, receiver_pid, socket, socket_type, protocol_type}).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start(non_neg_integer(), pid(), port(), atom(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, SocketType, ProtocolType) ->
    case gen_server:start_link({local, name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, SocketType, ProtocolType], []) of
        {error, {already_started, Pid}} ->
            %% replace socket
            gen_server:cast(Pid, {reconnect, ReceiverPid, Socket, SocketType, ProtocolType}),
            {ok, Pid};
        Result ->
            Result
    end.

%% @doc 获取角色写消息进程Pid
-spec pid(non_neg_integer() | pid()) ->pid() | undefined.
pid(RoleId) when is_integer(RoleId) ->
    process:pid(name(RoleId));
pid(Pid) when is_pid(Pid) ->
    Pid.

%% @doc 角色写消息进程名
-spec name(RoleId :: non_neg_integer()) -> atom().
name(RoleId) ->
    type:to_atom(lists:concat([role_sender_, RoleId])).

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: term()) -> ok.
send(#user{sender_pid = Pid}, Protocol, Data) ->
    {ok, Binary} = user_router:write(Protocol, Data),
    send(Pid, Binary);
send(Pid, Protocol, Data) when is_pid(Pid) ->
    {ok, Binary} = user_router:write(Protocol, Data),
    send(Pid, Binary);
send(RoleId, Protocol, Data) when is_integer(RoleId) ->
    {ok, Binary} = user_router:write(Protocol, Data),
    send(pid(RoleId), Binary).

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Binary :: binary()) -> ok.
send(_, <<>>) ->
    ok;
send(#user{sender_pid = Pid}, Binary) ->
    gen_server:cast(Pid, {send, Binary});
send(Pid, Binary) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Binary});
send(RoleId, Binary) when is_integer(RoleId) ->
    gen_server:cast(pid(RoleId), {send, Binary}).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([RoleId, ReceiverPid, Socket, SocketType, ProtocolType]) ->
    {ok, #state{role_id = RoleId, receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, protocol_type = ProtocolType}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Binary}, State = #state{socket_type = SocketType, socket = Socket, protocol_type = ProtocolType}) ->
    catch sender:send(Socket, SocketType, ProtocolType, Binary),
    {noreply, State};

handle_cast({reconnect, ReceiverPid, Socket, SocketType, ProtocolType}, State) ->
    {noreply, State#state{receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, protocol_type = ProtocolType}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
