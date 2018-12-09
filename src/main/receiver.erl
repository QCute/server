%%%-------------------------------------------------------------------
%%% @doc
%%% module receiver, to receive tcp data
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-behaviour(gen_server).
%% export API function
-export([start/4, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% socket state and socket error define
-include("common.hrl").
-include("socket.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(SocketType, Socket, Number, Increment) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number, "_", Increment])),
    ChildSpec = {Name, {?MODULE, start_link, [{Name, SocketType, Socket}]}, temporary, brutal_kill, worker, [Name]},
    main_supervisor:start_child(ChildSpec).

%% @doc server start
start_link({Name, SocketType, Socket}) ->
    gen_server:start_link({local, Name}, ?MODULE, [{SocketType, Socket}], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([{SocketType, Socket}]) ->
    gen_server:cast(self(), async_receive),
    {ok, #client{socket_type = SocketType, socket = Socket, reference = make_ref()}}.

handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast(async_receive, State) ->
    %% start async receive handler
    catch handle_receive(?PACK_HEAD_LENGTH, ?HEART_TIMEOUT, State),
    {noreply, State#client{state = wait_pack_first}};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({inet_async, Socket, _Ref, {ok, Data}}, State = #client{socket = Socket}) ->
    %% main receive & handle tpc data
    case catch reader:handle(State, Data) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        {read, Length, Timeout, NewState} ->
            handle_receive(Length, Timeout, NewState),
            {noreply, NewState};
        {continue, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
handle_info({inet_async, Socket, _Ref, {error, timeout}}, State = #client{socket = Socket}) ->
    %% tcp timeout
    handle_lost({disconnect, timeout}, State);
handle_info({inet_async, Socket, _Ref, {error, closed}}, State = #client{socket = Socket}) ->
    %% tcp closed
    handle_lost({disconnect, closed}, State);
handle_info({inet_async, _Socket, _Ref, _Msg}, State) ->
    %% other error state
    handle_lost({disconnect, reference_not_match}, State);
handle_info({'SEND', Binary}, State = #client{socket_type = gen_tcp, socket = Socket}) ->
    catch erts_internal:port_command(Socket, Binary, [force]),
    {noreply, State};
handle_info({'SEND', Binary}, State = #client{socket_type = ssl, socket = Socket}) ->
    catch ssl:send(Socket, Binary),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    {ok, State}.
code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% receive data
handle_receive(Length, Timeout, #client{socket = Socket, socket_type = gen_tcp}) ->
    prim_inet:async_recv(Socket, Length, Timeout);
handle_receive(Length, Timeout, #client{socket = Socket, socket_type = ssl}) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> erlang:send(Pid, {inet_async, Socket, Ref, catch ssl:recv(Socket, Length, Timeout)}) end);
handle_receive(_, _, _) ->
    ok.

%%%% client lost
handle_lost({disconnect, Reason}, State = #client{socket_type = gen_tcp, socket = Socket, user_pid = Pid}) ->
    %% logout/hold
    catch gen_server:cast(Pid, {'LOGOUT', Reason}),
    timer:sleep(100),
    %% close socket
    catch gen_tcp:close(Socket),
    {stop, normal, State};
handle_lost({disconnect, Reason}, State = #client{socket_type = ssl, socket = Socket, user_pid = Pid}) ->
    %% logout/hold
    catch gen_server:cast(Pid, {'LOGOUT', Reason}),
    timer:sleep(100),
    %% close socket
    catch ssl:close(Socket),
    {stop, normal, State};
handle_lost(_, State) ->
    {stop, normal, State}.