%%%-------------------------------------------------------------------
%%% @doc
%%% module role sender
%%% @end
%%%-------------------------------------------------------------------
-module(user_sender).
-behaviour(gen_server).
-compile({no_auto_import, [send/2]}).
%% API
-export([start/5, stop/1, send/2, send/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("user.hrl").
%% user sender state
-record(state, {role_id, receiver_pid, socket, socket_type, connect_type, connect_lost = false}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
-spec start(non_neg_integer(), pid(), port(), atom(), atom()) -> {ok, pid()} | {error, term()}.
start(RoleId, ReceiverPid, Socket, SocketType, ConnectType) ->
    gen_server:start({local, process:sender_name(RoleId)}, ?MODULE, [RoleId, ReceiverPid, Socket, SocketType, ConnectType], []).

%% @doc stop
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, 'stop').

%% @doc send to client use link sender
-spec send(#user{} | pid() | non_neg_integer(), Protocol :: non_neg_integer(), Data :: term()) -> ok.
send(_, _, []) ->
    ok;
send(Id, Protocol, Data) when is_integer(Id) ->
    send(process:sender_pid(Id), Protocol, Data);
send(#user{pid_sender = Pid}, Protocol, Data) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            erlang:send(Pid, {'send', Binary}),
            ok;
        _ ->
            {error, pack_data_error}
    end;
send(Pid, Protocol, Data) when is_pid(Pid) ->
    case user_router:write(Protocol, Data) of
        {ok, Binary} ->
            erlang:send(Pid, {'send', Binary}),
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
send(Id, Data) when is_integer(Id) ->
    send(process:sender_pid(Id), Data);
send(#user{pid_sender = Pid}, Binary) ->
    send(Pid, Binary),
    ok;
send(Pid, Binary) when is_pid(Pid) ->
    erlang:send(Pid, {'send', Binary}),
    ok;
send(_, _) ->
    ok.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([RoleId, ReceiverPid, Socket, SocketType, ConnectType]) ->
    {ok, #state{role_id = RoleId, receiver_pid = ReceiverPid, socket = Socket, socket_type = SocketType, connect_type = ConnectType}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast('stop', State = #state{socket_type = SocketType, socket = Socket}) ->
    %% handle stop
    %% close tcp socket
    catch SocketType:close(Socket),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'send', Binary}, State = #state{socket_type = SocketType, socket = Socket, connect_type = ConnectType}) ->
    catch sender:send(Socket, SocketType, ConnectType, Binary),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================