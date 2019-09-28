%%%-------------------------------------------------------------------
%%% @doc
%%% module acceptor
%%% @end
%%%-------------------------------------------------------------------
-module(acceptor).
-behaviour(gen_server).
%% API
-export([start/3, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state
-record(state, {socket_type, socket, reference, number = 0, increment = 0}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(SocketType, ListenSocket, Number) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType, "_", Number])),
    ChildSpec = {Name, {?MODULE, start_link, [[Name, SocketType, ListenSocket, Number]]}, permanent, 10000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
start_link([Name | Args]) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%====================================================================
%%% gen_server callback
%%%====================================================================
init([SocketType, ListenSocket, Number]) ->
    %% start accept
    gen_server:cast(self(), accept),
    {ok, #state{socket_type = SocketType, socket = ListenSocket, number = Number}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(accept, State = #state{socket_type = gen_tcp, socket = ListenSocket}) ->
    %% gen tcp async
    case catch prim_inet:async_accept(ListenSocket, -1) of
        {ok, Reference} ->
            {noreply, State#state{reference = Reference}};
        Error ->
            %% accept error force close
            {stop, {cannot_accept, Error}, State}
    end;
handle_cast(accept, State = #state{socket_type = ssl, socket = ListenSocket}) ->
    %% ssl
    Pid = self(),
    Reference = make_ref(),
    %% make spawn async accept
    spawn(fun() -> transport_accept(Pid, Reference, ListenSocket) end),
    {noreply, State#state{reference = Reference}};

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = gen_tcp, reference = Reference, socket = ListenSocket}) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    case catch prim_inet:getopts(ListenSocket, [active, keepalive, priority, tos]) of
        {ok, Options} ->
            case catch prim_inet:setopts(ListenSocket, Options) of
                ok ->
                    open_check(Socket, State);
                {error, Reason} ->
                    catch erlang:port_close(Socket),
                    exit({set_sockopt, Reason})
            end;
        {error, Reason} ->
            catch erlang:port_close(Socket),
            exit({get_sockopt, Reason})
    end;
handle_info({inet_async, ListenSocket, Reference, {ok, Socket}}, State = #state{socket_type = ssl, reference = Reference, socket = ListenSocket}) ->
    %% before ssl:ssl_accept()
    %% current ssl:handshake()
    case catch ssl:handshake(Socket) of
        ok ->
            case catch ssl:setopts(Socket, [{packet, 0}, {active, false}, {keepalive, false}]) of
                ok ->
                    open_check(Socket, State);
                {error, Reason} ->
                    catch ssl:close(Socket),
                    exit({set_sockopt, Reason})
            end;
        {error, Reason} ->
            catch ssl:close(Socket),
            exit({ssl_accept, Reason})
    end;
handle_info({inet_async, _, _, {error, closed}}, State) ->
    %% error state
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% async accept for ssl
transport_accept(Pid, Reference, ListenSocket) ->
    case catch ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case catch ssl:controlling_process(Socket, Pid) of
                ok ->
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, {ok, Socket}});
                _ ->
                    erlang:send(Pid, {inet_async, ListenSocket, Reference, {error, closed}})
            end;
        _ ->
            erlang:send(Pid, {inet_async, ListenSocket, Reference, {error, closed}})
    end.

%% handle socket after accept
open_check(Socket, State = #state{socket_type = SocketType}) ->
    %% control server open or not
    case catch user_manager:get_server_state() of
        {'EXIT', _} ->
            %% connect not permit
            catch SocketType:close(Socket),
            {noreply, State};
        refuse ->
            %% connect not permit
            catch SocketType:close(Socket),
            {noreply, State};
        ServerState ->
            start_receiver(Socket, State, ServerState)
    end.
start_receiver(Socket, State = #state{socket_type = SocketType, increment = Increment, number = Number}, ServerState) ->
    %% start child client
    case receiver:start(SocketType, Socket, Number, Increment, ServerState) of
        {ok, Child} ->
            control_process(Socket, Child, State#state{increment = Increment + 1});
        _ ->
            catch SocketType:close(Socket),
            {noreply, State}
    end.
control_process(Socket, Child, State = #state{socket_type = SocketType}) ->
    %% socket message send to process
    case catch SocketType:controlling_process(Socket, Child) of
        ok ->
            handle_cast(accept, State);
        Error ->
            catch SocketType:close(Socket),
            %% close child
            erlang:send(Child, {controlling_process, Error}),
            {noreply, State}
    end.
