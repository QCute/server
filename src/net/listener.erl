%%%------------------------------------------------------------------
%%% @doc
%%% module listener
%%% @end
%%%------------------------------------------------------------------
-module(listener).
-behaviour(gen_server).
%% API
-export([start_gen_tcp/0, start_ssl/0, start/0, start/2, start_link/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state
-record(state, {socket_type, socket, ref}).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc gen tcp daemon
-spec start_gen_tcp() -> {ok, pid()} | {error, term()}.
start_gen_tcp() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    Port = proplists:get_value(gen_tcp_port, List, 10000),
    start(gen_tcp, Port + Id).

%% @doc ssl daemon
-spec start_ssl() -> {ok, pid()} | {error, term()}.
start_ssl() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    Port = proplists:get_value(ssl_port, List, 10000),
    start(ssl, Port + Id).

%% @doc server start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    SocketType = proplists:get_value(socket_type, List, gen_tcp),
    PortType = type:to_atom(lists:concat([SocketType, "_port"])),
    Port = proplists:get_value(PortType, List, 10000),
    start(SocketType, Port + Id).

%% @doc start
-spec start(SocketType :: gen_tcp | ssl, Port :: inet:port_number()) -> {ok, pid()} | {error, term()}.
start(SocketType, Port) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType, Port]}, permanent, 10000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl, Port :: inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Name, SocketType, Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [SocketType, Port], []).
%%%==================================================================
%%% gen_server callback
%%%==================================================================
init([gen_tcp, Port]) ->
    erlang:process_flag(trap_exit, true),
    {ok, Net} = application:get_env(net),
    Number = proplists:get_value(gen_tcp_acceptor_number, Net, 1),
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            %% start tcp acceptor async
            gen_server:cast(self(), {start, Number}),
            {ok, #state{socket_type = gen_tcp, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end;
init([ssl, Port]) ->
    erlang:process_flag(trap_exit, true),
    {ok, Net} = application:get_env(net),
    Number = proplists:get_value(ssl_acceptor_number, Net, 1),
    CertFile = proplists:get_value(ssl_cert_file, Net, ""),
    KeyFile = proplists:get_value(ssl_key_file, Net, ""),
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {certfile, CertFile}, {keyfile, KeyFile}],
    case ssl:listen(Port, Options) of
        {ok, ListenSocket} ->
            %% start ssl acceptor async
            gen_server:cast(self(), {start, Number}),
            {ok, #state{socket_type = ssl, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({start, AcceptorNumber}, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    %% start tcp acceptor
    lists:foreach(fun(Number) -> acceptor:start(SocketType, ListenSocket, Number) end, lists:seq(1, AcceptorNumber)),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    catch SocketType:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
