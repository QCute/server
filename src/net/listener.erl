%%%-------------------------------------------------------------------
%%% @doc
%%% tcp listener
%%% @end
%%%-------------------------------------------------------------------
-module(listener).
%% API
-export([start/0]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> ok | {error, term()}.
start() ->
    select_type(config:net_socket_type()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% start
select_type(SocketType = gen_tcp) ->
    inets:start(),
    ServerId = config:server_id(),
    Config = config:net_gen_tcp(),
    AcceptorNumber = proplists:get_value(acceptor_number, Config, 1),
    StartPort = proplists:get_value(start_port, Config, 10000),
    select_mode(SocketType, StartPort + ServerId, AcceptorNumber, []);
select_type(SocketType = ssl) ->
    inets:start(),
    ssl:start(permanent),
    ServerId = config:server_id(),
    Config = config:net_ssl(),
    AcceptorNumber = proplists:get_value(acceptor_number, Config, 1),
    StartPort = proplists:get_value(start_port, Config, 20000),
    CertFile = proplists:get_value(cert_file, Config, ""),
    KeyFile = proplists:get_value(key_file, Config, ""),
    select_mode(SocketType, StartPort + ServerId, AcceptorNumber, [{certfile, CertFile}, {keyfile, KeyFile}]).

%% select normal or unix domain socket mode
select_mode(SocketType, Port, AcceptorNumber, Options) ->
    case config:net_uds_path() of
        [] ->
            %% normal mode
            listen(SocketType, Port, AcceptorNumber, Options);
        Path ->
            %% unix domain socket mode
            SocketFile = lists:concat([Path, "/", Port, ".sock"]),
            file:delete(SocketFile),
            listen(SocketType, 0, AcceptorNumber, [{ifaddr, {local, SocketFile}} | Options])
    end.

%% start listening
listen(SocketType, Port, AcceptorNumber, Options) ->
    BaseOptions = [{mode, binary}, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}],
    case SocketType:listen(Port, lists:merge(BaseOptions, Options)) of
        {ok, ListenSocket} ->
            %% start acceptor
            lists:foreach(fun(N) -> acceptor:start(SocketType, ListenSocket, N) end, lists:seq(1, AcceptorNumber));
        Error ->
            Error
    end.
