-module(config).
-compile(nowarn_export_all).
-compile(export_all).

mysql_driver_host() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(host, 1, Pool) of
                {host, Host} ->
                    Host;
                _ ->
                    "127.0.0.1"
            end;
        _ ->
            "127.0.0.1"
    end.

mysql_driver_port() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(port, 1, Pool) of
                {port, Port} ->
                    Port;
                _ ->
                    3306
            end;
        _ ->
            3306
    end.

mysql_driver_user() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(user, 1, Pool) of
                {user, User} ->
                    User;
                _ ->
                    "root"
            end;
        _ ->
            "root"
    end.

mysql_driver_password() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(password, 1, Pool) of
                {password, Password} ->
                    Password;
                _ ->
                    "root"
            end;
        _ ->
            "root"
    end.

mysql_driver_database() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(database, 1, Pool) of
                {database, Database} ->
                    Database;
                _ ->
                    "main"
            end;
        _ ->
            "main"
    end.

mysql_driver_encoding() ->
    case application:get_env(main, pool) of
        {ok, Pool} ->
            case lists:keyfind(encoding, 1, Pool) of
                {encoding, Encoding} ->
                    Encoding;
                _ ->
                    "utf8mb4"
            end;
        _ ->
            "utf8mb4"
    end.

net_ssl_file() ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_file, 1, Net) of
                {ssl_file, SslFile} ->
                    SslFile;
                _ ->
                    "cert/fake.me"
            end;
        _ ->
            "cert/fake.me"
    end.

net_gen_tcp_port() ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp_port, 1, Net) of
                {gen_tcp_port, GenTcpPort} ->
                    GenTcpPort;
                _ ->
                    10000
            end;
        _ ->
            10000
    end.

net_ssl_port() ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl_port, 1, Net) of
                {ssl_port, SslPort} ->
                    SslPort;
                _ ->
                    12000
            end;
        _ ->
            12000
    end.

net_socket_type() ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(socket_type, 1, Net) of
                {socket_type, SocketType} ->
                    SocketType;
                _ ->
                    gen_tcp
            end;
        _ ->
            gen_tcp
    end.

cookie() ->
    case application:get_env(main, cookie) of
        {ok, Cookie} ->
            Cookie;
        _ ->
            erlang
    end.

server_id() ->
    case application:get_env(main, server_id) of
        {ok, ServerId} ->
            ServerId;
        _ ->
            0
    end.

node_prefix() ->
    case application:get_env(main, node_prefix) of
        {ok, NodePrefix} ->
            NodePrefix;
        _ ->
            "erlang"
    end.

node_type() ->
    case application:get_env(main, node_type) of
        {ok, NodeType} ->
            NodeType;
        _ ->
            local
    end.

path_include() ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(include, 1, Path) of
                {include, Include} ->
                    Include;
                _ ->
                    "include/"
            end;
        _ ->
            "include/"
    end.

path_config() ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(config, 1, Path) of
                {config, Config} ->
                    Config;
                _ ->
                    "config/"
            end;
        _ ->
            "config/"
    end.

path_beam() ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(beam, 1, Path) of
                {beam, Beam} ->
                    Beam;
                _ ->
                    "beam/"
            end;
        _ ->
            "beam/"
    end.

path_src() ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(src, 1, Path) of
                {src, Src} ->
                    Src;
                _ ->
                    "src/"
            end;
        _ ->
            "src/"
    end.

