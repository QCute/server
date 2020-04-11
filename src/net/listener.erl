%%%-------------------------------------------------------------------
%%% @doc
%%% module listener
%%% @end
%%%-------------------------------------------------------------------
-module(listener).
-behaviour(gen_server).
%% API
-export([start_gen_tcp/0, start_ssl/0, start/1, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state
-record(state, {socket_type, socket, ref}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc gen tcp listen server
-spec start_gen_tcp() -> {ok, pid()} | {error, term()}.
start_gen_tcp() ->
    start(gen_tcp).

%% @doc ssl listen server
-spec start_ssl() -> {ok, pid()} | {error, term()}.
start_ssl() ->
    start(ssl).

%% @doc start
-spec start(SocketType :: gen_tcp | ssl) -> {ok, pid()} | {error, term()}.
start(SocketType) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType]}, permanent, 10000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl) -> {ok, pid()} | {error, term()}.
start_link(Name, SocketType) ->
    gen_server:start_link({local, Name}, ?MODULE, SocketType, []).
%%%===================================================================
%%% gen_server callback
%%%===================================================================
init(SocketType = gen_tcp) ->
    erlang:process_flag(trap_exit, true),
    {ok, Id} = application:get_env(server_id),
    {ok, Net} = application:get_env(net),
    Port = proplists:get_value(gen_tcp_start_port, Net, 10000),
    Number = proplists:get_value(gen_tcp_acceptor_number, Net, 1),
    Options = [{mode, binary}, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}],
    case gen_tcp:listen(Port + Id, Options) of
        {ok, ListenSocket} ->
            %% start tcp acceptor
            erlang:send(self(), {start_acceptor, Number}),
            {ok, #state{socket_type = SocketType, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end;
init(SocketType = ssl) ->
    erlang:process_flag(trap_exit, true),
    {ok, Id} = application:get_env(server_id),
    {ok, Net} = application:get_env(net),
    Port = proplists:get_value(ssl_start_port, Net, 20000),
    Number = proplists:get_value(ssl_acceptor_number, Net, 1),
    CertFile = proplists:get_value(ssl_cert_file, Net, ""),
    KeyFile = proplists:get_value(ssl_key_file, Net, ""),
    Options = [{mode, binary}, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}, {certfile, CertFile}, {keyfile, KeyFile}],
    case ssl:listen(Port + Id, Options) of
        {ok, ListenSocket} ->
            %% start ssl acceptor
            erlang:send(self(), {start_acceptor, Number}),
            {ok, #state{socket_type = SocketType, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({start_acceptor, 0}, State) ->
    {stop, normal, State};
handle_info({start_acceptor, Number}, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    lists:foreach(fun(N) -> acceptor:start(SocketType, ListenSocket, N) end, lists:seq(1, Number)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    catch SocketType:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
