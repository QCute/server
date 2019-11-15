%%%------------------------------------------------------------------
%%% @doc
%%% module receiver, to receive tcp data
%%% @end
%%%------------------------------------------------------------------
-module(receiver).
-behaviour(gen_server).
%% API
-export([start/4, start_link/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% socket state and socket error define
-include("socket.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
start(SocketType, Socket, Number, Increment) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number, "_", Increment])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType, Socket]}, temporary, brutal_kill, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
start_link(Name, SocketType, Socket) ->
    gen_server:start_link({local, Name}, ?MODULE, [SocketType, Socket], []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([SocketType, Socket]) ->
    erlang:send_after(1000, self(), async_receive),
    {ok, #client{socket_type = SocketType, socket = Socket}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({response, Binary}, State) ->
    %% http response
    catch sender:response(State, Binary),
    {noreply, State};
handle_cast({send, Binary}, State) ->
    %% send binary
    catch sender:send(State, Binary),
    {noreply, State};
handle_cast({stop, Response}, State) ->
    %% send response
    catch sender:send(State, Response),
    %% stop this receiver
    {stop, normal, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(async_receive, State) ->
    %% start async receive handler
    handle_receive(?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, State#client{state = wait_pack_first});
handle_info({inet_async, Socket, Ref, {ok, Data}}, State = #client{socket = Socket, reference = Ref}) ->
    %% main receive & handle tpc data
    case reader:handle(State, Data) of
        {continue, NewState} ->
            {noreply, NewState};
        {read, Length, Timeout, NewState} ->
            handle_receive(Length, Timeout, NewState);
        {stop, Reason, NewState} ->
            %% other reason is exception, need to report to error log
            {stop, Reason, NewState};
        _ ->
            {noreply, State}
    end;
handle_info({inet_async, Socket, Ref, {error, Reason}}, State = #client{socket = Socket, reference = Ref}) ->
    %% tcp timeout/closed
    {stop, {shutdown, Reason}, State};
handle_info({inet_async, _Socket, _Ref, _Msg}, State) ->
    %% ref not match
    {stop, {shutdown, ref_not_match}, State};
handle_info(Reason = {controlling_process, _Error}, State) ->
    %% controlling process error
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State = #client{socket_type = SocketType, socket = Socket, role_pid = RolePid}) ->
    %% report error
    catch gen_server:cast(RolePid, {disconnect, Reason}),
    %% close socket
    catch SocketType:close(Socket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
%% receive data
handle_receive(Length, Timeout, State = #client{socket = Socket, socket_type = gen_tcp}) ->
    case prim_inet:async_recv(Socket, Length, Timeout) of
        {ok, Ref} ->
            {noreply, State#client{reference = Ref}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_receive(Length, Timeout, State = #client{socket = Socket, socket_type = ssl}) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> erlang:send(Pid, {inet_async, Socket, Ref, catch ssl:recv(Socket, Length, Timeout)}) end),
    {noreply, State#client{reference = Ref}};
handle_receive(_, _, State) ->
    {noreply, State}.
