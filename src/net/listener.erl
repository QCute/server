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
%% state
-record(state, {socket_type, socket, ref}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc gen tcp daemon
start_gen_tcp() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    Port = proplists:get_value(gen_tcp_port, List, 10000),
    start(gen_tcp, Port + Id).

%% @doc ssl daemon
start_ssl() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    Port = proplists:get_value(ssl_port, List, 10000),
    start(ssl, Port + Id).

%% @doc server start
start() ->
    {ok, Id} = application:get_env(server_id),
    {ok, List} = application:get_env(net),
    SocketType = proplists:get_value(socket_type, List, gen_tcp),
    PortType = type:to_atom(lists:concat([SocketType, "_port"])),
    Port = proplists:get_value(PortType, List, 10000),
    start(SocketType, Port + Id).

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
    {ok, Path} = application:get_env(path),
    FilePath = proplists:get_value(config, Path, "config/"),
    {ok, Net} = application:get_env(net),
    FileName = proplists:get_value(ssl_file, Net, ""),
    CertFile = lists:concat([FilePath, FileName, ".crt"]),
    KeyFile = lists:concat([FilePath, FileName, ".key"]),
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {certfile, CertFile}, {keyfile, KeyFile}],
    case ssl:listen(Port, Options) of
        {ok, ListenSocket} ->
            %% start tcp acceptor async
            gen_server:cast(self(), {start, 16}),
            {ok, #state{socket_type = ssl, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({start, AcceptorAmount}, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    %% start tcp acceptor
    lists:foreach(fun(Number) -> acceptor:start(SocketType, ListenSocket, Number) end, lists:seq(1, AcceptorAmount)),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    catch SocketType:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
