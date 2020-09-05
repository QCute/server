%%%-------------------------------------------------------------------
%%% @doc
%%% tcp listener
%%% @end
%%%-------------------------------------------------------------------
-module(listener).
-behaviour(gen_server).
%% API
-export([start/0, start/1, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state
-record(state, {socket_type, socket}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    {ok, Net} = application:get_env(net),
    start(proplists:get_value(socket_type, Net, gen_tcp)).

%% @doc start
-spec start(SocketType :: gen_tcp | ssl) -> {ok, pid()} | {error, term()}.
start(SocketType) ->
    Name = list_to_atom(lists:concat([?MODULE, "_", SocketType])),
    ChildSpec = {Name, {?MODULE, start_link, [Name, SocketType]}, permanent, 60000, worker, [Name]},
    net_supervisor:start_child(ChildSpec).

%% @doc server start
-spec start_link(Name :: atom(), SocketType :: gen_tcp | ssl) -> {ok, pid()} | {error, term()}.
start_link(Name, SocketType) ->
    gen_server:start_link({local, Name}, ?MODULE, SocketType, []).

%%%===================================================================
%%% gen_server callback
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #state{}} | {stop, {cannot_listen, term()}}.
init(SocketType = gen_tcp) ->
    erlang:process_flag(trap_exit, true),
    inets:start(),
    {ok, ServerId} = application:get_env(server_id),
    {ok, Net} = application:get_env(net),
    StartPort = proplists:get_value(gen_tcp_start_port, Net, 10000),
    chose_mode(SocketType, StartPort + ServerId, [], Net);
init(SocketType = ssl) ->
    erlang:process_flag(trap_exit, true),
    inets:start(),
    ssl:start(permanent),
    {ok, ServerId} = application:get_env(server_id),
    {ok, Net} = application:get_env(net),
    StartPort = proplists:get_value(ssl_start_port, Net, 20000),
    CertFile = proplists:get_value(ssl_cert_file, Net, ""),
    KeyFile = proplists:get_value(ssl_key_file, Net, ""),
    chose_mode(SocketType, StartPort + ServerId, [{certfile, CertFile}, {keyfile, KeyFile}], Net).

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, Reply :: term(), NewState :: #state{}}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info({start_acceptor, Number}, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    lists:foreach(fun(N) -> acceptor:start(SocketType, ListenSocket, N) end, lists:seq(1, Number)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> {ok, NewState :: #state{}}.
terminate(_Reason, State = #state{socket_type = SocketType, socket = ListenSocket}) ->
    catch SocketType:close(ListenSocket),
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% chose normal or unix mode
chose_mode(SocketType, Port, Options, Config) ->
    case proplists:get_value(uds_path, Config, "") of
        [] ->
            %% normal mode
            start_listen(SocketType, Port, Options, Config);
        Path ->
            %% unix domain socket mode
            SocketFile = lists:concat([Path, "/", Port, ".sock"]),
            ok = filelib:ensure_dir(Path),
            ok = file:delete(SocketFile),
            start_listen(SocketType, 0, [{ifaddr, {local, SocketFile}} | Options], Config)
    end.

%% start listening
start_listen(SocketType, Port, Options, Config) ->
    BaseOptions = [{mode, binary}, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}],
    case SocketType:listen(Port, lists:merge(BaseOptions, Options)) of
        {ok, ListenSocket} ->
            %% start ssl acceptor
            AcceptorNumber = proplists:get_value(ssl_acceptor_number, Config, 1),
            erlang:send(self(), {start_acceptor, AcceptorNumber}),
            {ok, #state{socket_type = SocketType, socket = ListenSocket}};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.
