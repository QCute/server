%%%-------------------------------------------------------------------
%%% @doc
%%% module role sender
%%% @end
%%%-------------------------------------------------------------------
-module(user_sender).
-behaviour(gen_server).
-compile({no_auto_import, [send/2, send/3]}).
%% API
-export([start/5, stop/1]).
-export([send/2, send/3]).
-export([send_delay/5, push_delayed/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("user.hrl").
%% user sender state
-record(state, {role_id, receiver_pid, socket, socket_type, connect_type, connect_lost = false, keep = []}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
-spec start(non_neg_integer(), pid(), port(), atom(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, SocketType, ConnectType) ->
    gen_server:start({local, sender_name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, SocketType, ConnectType], []).

%% @doc stop
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, 'stop').

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: term()) -> ok.
send(_, _, []) ->
    ok;
send(RoleId, Protocol, Data) when is_integer(RoleId) ->
    send(sender_pid(RoleId), Protocol, Data);
send(#user{sender_pid = Pid}, Protocol, Data) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            send(Pid, {'send', Binary}),
            ok;
        _ ->
            {error, pack_data_error}
    end;
send(Pid, Protocol, Data) when is_pid(Pid) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            send(Pid, {'send', Binary}),
            ok;
        _ ->
            {error, pack_data_error}
    end;
send(_, _, _) ->
    ok.

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Binary :: binary()) -> ok.
send(_, <<>>) ->
    ok;
send(RoleId, Data) when is_integer(RoleId) ->
    gen_server:cast(sender_pid(RoleId), Data);
send(#user{sender_pid = Pid}, Binary) ->
    gen_server:cast(Pid, Binary),
    ok;
send(Pid, Binary) when is_pid(Pid) ->
    gen_server:cast(Pid, {'send', Binary}),
    ok;
send(_, _) ->
    ok.

%% @doc send delay
-spec send_delay(#user{} | pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: term(), Id :: non_neg_integer(), Timeout :: timeout()) -> ok.
send_delay(_, _, [], _, _) ->
    ok;
send_delay(RoleId, Protocol, Data, Id, Timeout) when is_integer(RoleId) ->
    send_delay(sender_pid(RoleId), Protocol, Data, Id, Timeout);
send_delay(#user{sender_pid = Pid}, Protocol, Data, Id, Timeout) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            send(Pid, {'send', Binary, Id, Timeout}),
            ok;
        _ ->
            {error, pack_data_error}
    end;
send_delay(Pid, Protocol, Data, Id, Timeout) when is_pid(Pid) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            send(Pid, {'send', Binary, Id, Timeout}),
            ok;
        _ ->
            {error, pack_data_error}
    end;
send_delay(_, _, _, _, _) ->
    ok.

%% @doc send delayed binary at now
-spec push_delayed(#user{} | pid() | non_neg_integer(), Id :: non_neg_integer()) -> ok.
push_delayed(RoleId, Id) when is_integer(RoleId) ->
    send(sender_pid(RoleId), {send_timeout, Id});
push_delayed(#user{sender_pid = Pid}, Id) ->
    send(Pid, {send_timeout, Id});
push_delayed(Pid, Id) when is_pid(Pid) ->
    send(Pid, {send_timeout, Id});
push_delayed(_, _) ->
    ok.

%% @doc 获取角色写消息进程Pid
-spec sender_pid(non_neg_integer() | pid()) -> Pid :: pid() | undefined.
sender_pid(Pid) when is_pid(Pid) ->
    Pid;
sender_pid(RoleId) when is_integer(RoleId) ->
    process:where(sender_name(RoleId)).

%% @doc 角色写消息进程名
-spec sender_name(RoleId :: non_neg_integer()) -> atom().
sender_name(RoleId) ->
    type:to_atom(lists:concat([role_sender_, RoleId])).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([RoleId, ReceiverPid, Socket, SocketType, ConnectType]) ->
    {ok, #state{role_id = RoleId, receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, connect_type = ConnectType}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({'send', Binary}, State = #state{socket_type = SocketType, socket = Socket, connect_type = ConnectType}) ->
    catch sender:send(Socket, SocketType, ConnectType, Binary),
    {noreply, State};
handle_cast({'send_delay', Binary, Id, Timeout}, State = #state{keep = Keep}) ->
    Timer = erlang:send_after(Timeout, self(), {send_timeout, Id}),
    NewKeep = [{Id, Binary, Timer} | Keep],
    {noreply, State#state{keep = NewKeep}};
handle_cast({send_timeout, Id}, State = #state{socket_type = SocketType, socket = Socket, connect_type = ConnectType, keep = Keep}) ->
    case lists:keytake(Id, 1, Keep) of
        {value, {_, Binary, Ref}, NewKeep} ->
            %% cancel timer is useful when call function push_delayed
            %% of course, it is useless when timer timeout
            catch erlang:cancel_timer(Ref),
            catch sender:send(Socket, SocketType, ConnectType, Binary),
            {noreply, State#state{keep = NewKeep}};
        false ->
            {noreply, State}
    end;
handle_cast('stop', State = #state{socket_type = SocketType, socket = Socket}) ->
    %% handle stop
    %% close tcp socket
    catch SocketType:close(Socket),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================