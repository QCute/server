-module(config).
-compile(nowarn_export_all).
-compile(export_all).

net() ->
    net([{uds_path,[]},{socket_type,gen_tcp},{gen_tcp_start_port,10000},{gen_tcp_acceptor_number,1},{ssl_start_port,20000},{ssl_acceptor_number,1},{ssl_cert_file,"config/cert/fake.me.crt"},{ssl_key_file,"config/cert/fake.me.key"}]).

net(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            Net;
        _ ->
            Default
    end.

net_uds_path() ->
    net_uds_path([]).

net_uds_path(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(uds_path, 1, Net) of
                {uds_path, UdsPath} ->
                    UdsPath;
                _ ->
                    []
            end;
        _ ->
            Default
    end.

net_socket_type() ->
    net_socket_type(gen_tcp).

net_socket_type(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(socket_type, 1, Net) of
                {socket_type, SocketType} ->
                    SocketType;
                _ ->
                    gen_tcp
            end;
        _ ->
            Default
    end.

net_gen_tcp_start_port() ->
    net_gen_tcp_start_port(10000).

net_gen_tcp_start_port(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp_start_port, 1, Net) of
                {gen_tcp_start_port, GenTcpStartPort} ->
                    GenTcpStartPort;
                _ ->
                    10000
            end;
        _ ->
            Default
    end.

net_gen_tcp_acceptor_number() ->
    net_gen_tcp_acceptor_number(1).

net_gen_tcp_acceptor_number(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp_acceptor_number, 1, Net) of
                {gen_tcp_acceptor_number, GenTcpAcceptorNumber} ->
                    GenTcpAcceptorNumber;
                _ ->
                    1
            end;
        _ ->
            Default
    end.

net_ssl_start_port() ->
    net_ssl_start_port(20000).

net_ssl_start_port(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_start_port, 1, Net) of
                {ssl_start_port, SslStartPort} ->
                    SslStartPort;
                _ ->
                    20000
            end;
        _ ->
            Default
    end.

net_ssl_acceptor_number() ->
    net_ssl_acceptor_number(1).

net_ssl_acceptor_number(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_acceptor_number, 1, Net) of
                {ssl_acceptor_number, SslAcceptorNumber} ->
                    SslAcceptorNumber;
                _ ->
                    1
            end;
        _ ->
            Default
    end.

net_ssl_cert_file() ->
    net_ssl_cert_file("config/cert/fake.me.crt").

net_ssl_cert_file(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_cert_file, 1, Net) of
                {ssl_cert_file, SslCertFile} ->
                    SslCertFile;
                _ ->
                    "config/cert/fake.me.crt"
            end;
        _ ->
            Default
    end.

net_ssl_key_file() ->
    net_ssl_key_file("config/cert/fake.me.key").

net_ssl_key_file(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_key_file, 1, Net) of
                {ssl_key_file, SslKeyFile} ->
                    SslKeyFile;
                _ ->
                    "config/cert/fake.me.key"
            end;
        _ ->
            Default
    end.

path_finder_pool() ->
    path_finder_pool([{size,1}]).

path_finder_pool(Default) ->
    case application:get_env(main, path_finder_pool) of
        {ok, PathFinderPool} ->
            PathFinderPool;
        _ ->
            Default
    end.

path_finder_pool_size() ->
    path_finder_pool_size(1).

path_finder_pool_size(Default) ->
    case application:get_env(main, path_finder_pool) of
        {ok, PathFinderPool} ->
            case lists:keyfind(size, 1, PathFinderPool) of
                {size, Size} ->
                    Size;
                _ ->
                    1
            end;
        _ ->
            Default
    end.

mysql_connector_pool() ->
    mysql_connector_pool([{size,1}]).

mysql_connector_pool(Default) ->
    case application:get_env(main, mysql_connector_pool) of
        {ok, MysqlConnectorPool} ->
            MysqlConnectorPool;
        _ ->
            Default
    end.

mysql_connector_pool_size() ->
    mysql_connector_pool_size(1).

mysql_connector_pool_size(Default) ->
    case application:get_env(main, mysql_connector_pool) of
        {ok, MysqlConnectorPool} ->
            case lists:keyfind(size, 1, MysqlConnectorPool) of
                {size, Size} ->
                    Size;
                _ ->
                    1
            end;
        _ ->
            Default
    end.

mysql_connector() ->
    mysql_connector([{host,"127.0.0.1"},{port,3306},{user,"root"},{password,"root"},{database,"local"},{encoding,"utf8mb4"}]).

mysql_connector(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            MysqlConnector;
        _ ->
            Default
    end.

mysql_connector_host() ->
    mysql_connector_host("127.0.0.1").

mysql_connector_host(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(host, 1, MysqlConnector) of
                {host, Host} ->
                    Host;
                _ ->
                    "127.0.0.1"
            end;
        _ ->
            Default
    end.

mysql_connector_port() ->
    mysql_connector_port(3306).

mysql_connector_port(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(port, 1, MysqlConnector) of
                {port, Port} ->
                    Port;
                _ ->
                    3306
            end;
        _ ->
            Default
    end.

mysql_connector_user() ->
    mysql_connector_user("root").

mysql_connector_user(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(user, 1, MysqlConnector) of
                {user, User} ->
                    User;
                _ ->
                    "root"
            end;
        _ ->
            Default
    end.

mysql_connector_password() ->
    mysql_connector_password("root").

mysql_connector_password(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(password, 1, MysqlConnector) of
                {password, Password} ->
                    Password;
                _ ->
                    "root"
            end;
        _ ->
            Default
    end.

mysql_connector_database() ->
    mysql_connector_database("local").

mysql_connector_database(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(database, 1, MysqlConnector) of
                {database, Database} ->
                    Database;
                _ ->
                    "local"
            end;
        _ ->
            Default
    end.

mysql_connector_encoding() ->
    mysql_connector_encoding("utf8mb4").

mysql_connector_encoding(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(encoding, 1, MysqlConnector) of
                {encoding, Encoding} ->
                    Encoding;
                _ ->
                    "utf8mb4"
            end;
        _ ->
            Default
    end.

cookie() ->
    cookie(erlang).

cookie(Default) ->
    case application:get_env(main, cookie) of
        {ok, Cookie} ->
            Cookie;
        _ ->
            Default
    end.

node_type() ->
    node_type(local).

node_type(Default) ->
    case application:get_env(main, node_type) of
        {ok, NodeType} ->
            NodeType;
        _ ->
            Default
    end.

server_id() ->
    server_id(1000).

server_id(Default) ->
    case application:get_env(main, server_id) of
        {ok, ServerId} ->
            ServerId;
        _ ->
            Default
    end.

server_id_list() ->
    server_id_list([]).

server_id_list(Default) ->
    case application:get_env(main, server_id_list) of
        {ok, ServerIdList} ->
            ServerIdList;
        _ ->
            Default
    end.

open_time() ->
    open_time(1577808000).

open_time(Default) ->
    case application:get_env(main, open_time) of
        {ok, OpenTime} ->
            OpenTime;
        _ ->
            Default
    end.

center_node() ->
    center_node(center).

center_node(Default) ->
    case application:get_env(main, center_node) of
        {ok, CenterNode} ->
            CenterNode;
        _ ->
            Default
    end.

center_ip() ->
    center_ip([]).

center_ip(Default) ->
    case application:get_env(main, center_ip) of
        {ok, CenterIp} ->
            CenterIp;
        _ ->
            Default
    end.

world_node() ->
    world_node(world).

world_node(Default) ->
    case application:get_env(main, world_node) of
        {ok, WorldNode} ->
            WorldNode;
        _ ->
            Default
    end.

world_ip() ->
    world_ip([]).

world_ip(Default) ->
    case application:get_env(main, world_ip) of
        {ok, WorldIp} ->
            WorldIp;
        _ ->
            Default
    end.

log_retain_file() ->
    log_retain_file([]).

log_retain_file(Default) ->
    case application:get_env(main, log_retain_file) of
        {ok, LogRetainFile} ->
            LogRetainFile;
        _ ->
            Default
    end.

path() ->
    path([{app,"config/app/"},{beam,"beam/"},{config,"config/"},{include,"include/"},{script,"script/"},{src,"src/"}]).

path(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            Path;
        _ ->
            Default
    end.

path_app() ->
    path_app("config/app/").

path_app(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(app, 1, Path) of
                {app, App} ->
                    App;
                _ ->
                    "config/app/"
            end;
        _ ->
            Default
    end.

path_beam() ->
    path_beam("beam/").

path_beam(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(beam, 1, Path) of
                {beam, Beam} ->
                    Beam;
                _ ->
                    "beam/"
            end;
        _ ->
            Default
    end.

path_config() ->
    path_config("config/").

path_config(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(config, 1, Path) of
                {config, Config} ->
                    Config;
                _ ->
                    "config/"
            end;
        _ ->
            Default
    end.

path_include() ->
    path_include("include/").

path_include(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(include, 1, Path) of
                {include, Include} ->
                    Include;
                _ ->
                    "include/"
            end;
        _ ->
            Default
    end.

path_script() ->
    path_script("script/").

path_script(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(script, 1, Path) of
                {script, Script} ->
                    Script;
                _ ->
                    "script/"
            end;
        _ ->
            Default
    end.

path_src() ->
    path_src("src/").

path_src(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(src, 1, Path) of
                {src, Src} ->
                    Src;
                _ ->
                    "src/"
            end;
        _ ->
            Default
    end.


%% @doc format config file
-spec format(File :: string()) -> ok.
format(File) ->
    {ok, [Config]} = file:consult(File),
    file:write_file(File, lists:concat([format_config(Config, 1, []), "."])).
format_config([], Depth, String) ->
    Padding = lists:concat(lists:duplicate((Depth - 1) * 4, " ")),
    lists:concat(["[\n", string:join(lists:reverse(String), ",\n"), "\n", Padding, "]"]);
format_config([{Key, Value} | T], Depth, String) ->
    case is_list(Value) andalso lists:any(fun(C) -> is_tuple(C) end, Value) of
        true ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            NewString = io_lib:format("~s{~p,~s}", [Padding, Key, format_config(Value, Depth + 1, [])]),
            format_config(T, Depth, [NewString | String]);
        false when Value == [] ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            Align = lists:concat(lists:duplicate(53 - (Depth * 4 + 1 + length(lists:concat([Key]))), " ")),
            NewString = io_lib:format("~s{~p,~s\"\"}", [Padding, Key, Align]),
            format_config(T, Depth, [NewString | String]);
        false ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            Align = lists:concat(lists:duplicate(53 - (Depth * 4 + 1 + length(lists:concat([Key]))), " ")),
            NewString = io_lib:format("~s{~p,~s~p}", [Padding, Key, Align, Value]),
            format_config(T, Depth, [NewString | String])
    end.
