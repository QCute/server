%%%-------------------------------------------------------------------
%%% @doc
%%% module listener
%%% @end
%%%-------------------------------------------------------------------
-module(listener).
-behaviour(gen_server).
%% export API function
-export([start_gen_tcp/0, start_ssl/0, start/0, start/2, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
%% state
-record(state, {socket_type, socket, ref}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc gen tcp daemon
start_gen_tcp() ->
    Port = type:to_integer(element(2, application:get_env(gen_tcp_port))) + type:to_integer(element(2, application:get_env(server_no))),
    start(gen_tcp, Port).

%% @doc ssl daemon
start_ssl() ->
    Port = type:to_integer(element(2, application:get_env(ssl_port))) + type:to_integer(element(2, application:get_env(server_no))),
    start(ssl, Port).

%% @doc server start
start() ->
    SocketType = type:to_atom(element(2, application:get_env(socket_type))),
    PortParam = type:to_atom(lists:concat([SocketType, "_port"])),
    Port = type:to_integer(element(2, application:get_env(PortParam))) + type:to_integer(element(2, application:get_env(server_no))),
    start(SocketType, Port).

start(SocketType, Port) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType])),
    ChildSpec = {Name, {?MODULE, start_link, [[Name, SocketType, Port]]}, permanent, 10000, worker, [Name]},
    main_supervisor:start_child(ChildSpec).

%% @doc server start
start_link([Name | Args]) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%====================================================================
%% gen_server callback
%%====================================================================
init([gen_tcp, Port]) ->
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            %% start tcp acceptor async
            gen_server:cast(self(), {start, 16}),
            {ok, #state{socket_type = gen_tcp, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end;
init([ssl, Port]) ->
    CertFile = lists:concat([element(2, application:get_env(ssl_file)), ".crt"]),
    KeyFile = lists:concat([element(2, application:get_env(ssl_file)), ".key"]),
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {certfile, CertFile}, {keyfile, KeyFile}],
    case ssl:listen(Port, Options) of
        {ok, ListenSocket} ->
            %% start tcp acceptor async
            gen_server:cast(self(), {start, 16}),
            {ok, #state{socket_type = ssl, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.

handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast({start, AcceptorAmount}, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    %% start tcp acceptor
    lists:foreach(fun(Number) -> acceptor:start(SocketType, ListenSocket, Number) end, lists:seq(1, AcceptorAmount)),
    {noreply, State};
handle_cast(_Info, State)->
    {noreply, State}.

handle_info(_Info, State)->
    {noreply, State}.

terminate(normal, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    catch SocketType:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
