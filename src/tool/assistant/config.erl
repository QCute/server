-module(config).
-export([net/0, net/1]).
-export([net_uds_path/0, net_uds_path/1]).
-export([net_socket_type/0, net_socket_type/1]).
-export([net_gen_tcp/0, net_gen_tcp/1]).
-export([net_gen_tcp_start_port/0, net_gen_tcp_start_port/1]).
-export([net_gen_tcp_acceptor_number/0, net_gen_tcp_acceptor_number/1]).
-export([net_ssl/0, net_ssl/1]).
-export([net_ssl_start_port/0, net_ssl_start_port/1]).
-export([net_ssl_acceptor_number/0, net_ssl_acceptor_number/1]).
-export([net_ssl_cert_file/0, net_ssl_cert_file/1]).
-export([net_ssl_key_file/0, net_ssl_key_file/1]).
-export([path_finder_pool/0, path_finder_pool/1]).
-export([path_finder_pool_size/0, path_finder_pool_size/1]).
-export([mysql_connector_pool/0, mysql_connector_pool/1]).
-export([mysql_connector_pool_size/0, mysql_connector_pool_size/1]).
-export([mysql_connector/0, mysql_connector/1]).
-export([mysql_connector_host/0, mysql_connector_host/1]).
-export([mysql_connector_port/0, mysql_connector_port/1]).
-export([mysql_connector_user/0, mysql_connector_user/1]).
-export([mysql_connector_password/0, mysql_connector_password/1]).
-export([mysql_connector_database/0, mysql_connector_database/1]).
-export([mysql_connector_encoding/0, mysql_connector_encoding/1]).
-export([cookie/0, cookie/1]).
-export([node_type/0, node_type/1]).
-export([server_id/0, server_id/1]).
-export([server_id_list/0, server_id_list/1]).
-export([open_time/0, open_time/1]).
-export([center_node/0, center_node/1]).
-export([center_ip/0, center_ip/1]).
-export([world_node/0, world_node/1]).
-export([world_ip/0, world_ip/1]).
-export([log_retain_file/0, log_retain_file/1]).
-export([path/0, path/1]).
-export([path_app/0, path_app/1]).
-export([path_beam/0, path_beam/1]).
-export([path_config/0, path_config/1]).
-export([path_include/0, path_include/1]).
-export([path_script/0, path_script/1]).
-export([path_src/0, path_src/1]).


-spec net() -> proplists:proplist().
net() ->
    net([{uds_path,[]},{socket_type,gen_tcp},{gen_tcp,[{start_port,10000},{acceptor_number,1}]},{ssl,[{start_port,20000},{acceptor_number,1},{cert_file,"config/cert/local.host/local.host.crt"},{key_file,"config/cert/local.host/local.host.key"}]}]).

-spec net(Default :: proplists:proplist()) -> proplists:proplist().
net(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            Net;
        _ ->
            Default
    end.

-spec net_uds_path() -> string().
net_uds_path() ->
    net_uds_path([]).

-spec net_uds_path(Default :: string()) -> string().
net_uds_path(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(uds_path, 1, Net) of
                {uds_path, UdsPath} ->
                    UdsPath;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_socket_type() -> atom().
net_socket_type() ->
    net_socket_type(gen_tcp).

-spec net_socket_type(Default :: atom()) -> atom().
net_socket_type(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(socket_type, 1, Net) of
                {socket_type, SocketType} ->
                    SocketType;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_gen_tcp() -> proplists:proplist().
net_gen_tcp() ->
    net_gen_tcp([{start_port,10000},{acceptor_number,1}]).

-spec net_gen_tcp(Default :: proplists:proplist()) -> proplists:proplist().
net_gen_tcp(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp, 1, Net) of
                {gen_tcp, GenTcp} ->
                    GenTcp;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_gen_tcp_start_port() -> integer().
net_gen_tcp_start_port() ->
    net_gen_tcp_start_port(10000).

-spec net_gen_tcp_start_port(Default :: integer()) -> integer().
net_gen_tcp_start_port(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp, 1, Net) of
                {gen_tcp, GenTcp} ->
                    case lists:keyfind(start_port, 1, GenTcp) of
                        {start_port, StartPort} ->
                            StartPort;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_gen_tcp_acceptor_number() -> integer().
net_gen_tcp_acceptor_number() ->
    net_gen_tcp_acceptor_number(1).

-spec net_gen_tcp_acceptor_number(Default :: integer()) -> integer().
net_gen_tcp_acceptor_number(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(gen_tcp, 1, Net) of
                {gen_tcp, GenTcp} ->
                    case lists:keyfind(acceptor_number, 1, GenTcp) of
                        {acceptor_number, AcceptorNumber} ->
                            AcceptorNumber;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_ssl() -> proplists:proplist().
net_ssl() ->
    net_ssl([{start_port,20000},{acceptor_number,1},{cert_file,"config/cert/local.host/local.host.crt"},{key_file,"config/cert/local.host/local.host.key"}]).

-spec net_ssl(Default :: proplists:proplist()) -> proplists:proplist().
net_ssl(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl, 1, Net) of
                {ssl, Ssl} ->
                    Ssl;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_ssl_start_port() -> integer().
net_ssl_start_port() ->
    net_ssl_start_port(20000).

-spec net_ssl_start_port(Default :: integer()) -> integer().
net_ssl_start_port(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl, 1, Net) of
                {ssl, Ssl} ->
                    case lists:keyfind(start_port, 1, Ssl) of
                        {start_port, StartPort} ->
                            StartPort;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_ssl_acceptor_number() -> integer().
net_ssl_acceptor_number() ->
    net_ssl_acceptor_number(1).

-spec net_ssl_acceptor_number(Default :: integer()) -> integer().
net_ssl_acceptor_number(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl, 1, Net) of
                {ssl, Ssl} ->
                    case lists:keyfind(acceptor_number, 1, Ssl) of
                        {acceptor_number, AcceptorNumber} ->
                            AcceptorNumber;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_ssl_cert_file() -> string().
net_ssl_cert_file() ->
    net_ssl_cert_file("config/cert/local.host/local.host.crt").

-spec net_ssl_cert_file(Default :: string()) -> string().
net_ssl_cert_file(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl, 1, Net) of
                {ssl, Ssl} ->
                    case lists:keyfind(cert_file, 1, Ssl) of
                        {cert_file, CertFile} ->
                            CertFile;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec net_ssl_key_file() -> string().
net_ssl_key_file() ->
    net_ssl_key_file("config/cert/local.host/local.host.key").

-spec net_ssl_key_file(Default :: string()) -> string().
net_ssl_key_file(Default) ->
    case application:get_env(main, net) of
        {ok, Net} ->
            case lists:keyfind(ssl, 1, Net) of
                {ssl, Ssl} ->
                    case lists:keyfind(key_file, 1, Ssl) of
                        {key_file, KeyFile} ->
                            KeyFile;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_finder_pool() -> proplists:proplist().
path_finder_pool() ->
    path_finder_pool([{size,1}]).

-spec path_finder_pool(Default :: proplists:proplist()) -> proplists:proplist().
path_finder_pool(Default) ->
    case application:get_env(main, path_finder_pool) of
        {ok, PathFinderPool} ->
            PathFinderPool;
        _ ->
            Default
    end.

-spec path_finder_pool_size() -> integer().
path_finder_pool_size() ->
    path_finder_pool_size(1).

-spec path_finder_pool_size(Default :: integer()) -> integer().
path_finder_pool_size(Default) ->
    case application:get_env(main, path_finder_pool) of
        {ok, PathFinderPool} ->
            case lists:keyfind(size, 1, PathFinderPool) of
                {size, Size} ->
                    Size;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_pool() -> proplists:proplist().
mysql_connector_pool() ->
    mysql_connector_pool([{size,1}]).

-spec mysql_connector_pool(Default :: proplists:proplist()) -> proplists:proplist().
mysql_connector_pool(Default) ->
    case application:get_env(main, mysql_connector_pool) of
        {ok, MysqlConnectorPool} ->
            MysqlConnectorPool;
        _ ->
            Default
    end.

-spec mysql_connector_pool_size() -> integer().
mysql_connector_pool_size() ->
    mysql_connector_pool_size(1).

-spec mysql_connector_pool_size(Default :: integer()) -> integer().
mysql_connector_pool_size(Default) ->
    case application:get_env(main, mysql_connector_pool) of
        {ok, MysqlConnectorPool} ->
            case lists:keyfind(size, 1, MysqlConnectorPool) of
                {size, Size} ->
                    Size;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector() -> proplists:proplist().
mysql_connector() ->
    mysql_connector([{host,"127.0.0.1"},{port,3306},{user,"root"},{password,"root"},{database,"local"},{encoding,"utf8mb4"}]).

-spec mysql_connector(Default :: proplists:proplist()) -> proplists:proplist().
mysql_connector(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            MysqlConnector;
        _ ->
            Default
    end.

-spec mysql_connector_host() -> string().
mysql_connector_host() ->
    mysql_connector_host("127.0.0.1").

-spec mysql_connector_host(Default :: string()) -> string().
mysql_connector_host(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(host, 1, MysqlConnector) of
                {host, Host} ->
                    Host;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_port() -> integer().
mysql_connector_port() ->
    mysql_connector_port(3306).

-spec mysql_connector_port(Default :: integer()) -> integer().
mysql_connector_port(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(port, 1, MysqlConnector) of
                {port, Port} ->
                    Port;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_user() -> string().
mysql_connector_user() ->
    mysql_connector_user("root").

-spec mysql_connector_user(Default :: string()) -> string().
mysql_connector_user(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(user, 1, MysqlConnector) of
                {user, User} ->
                    User;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_password() -> string().
mysql_connector_password() ->
    mysql_connector_password("root").

-spec mysql_connector_password(Default :: string()) -> string().
mysql_connector_password(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(password, 1, MysqlConnector) of
                {password, Password} ->
                    Password;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_database() -> string().
mysql_connector_database() ->
    mysql_connector_database("local").

-spec mysql_connector_database(Default :: string()) -> string().
mysql_connector_database(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(database, 1, MysqlConnector) of
                {database, Database} ->
                    Database;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec mysql_connector_encoding() -> string().
mysql_connector_encoding() ->
    mysql_connector_encoding("utf8mb4").

-spec mysql_connector_encoding(Default :: string()) -> string().
mysql_connector_encoding(Default) ->
    case application:get_env(main, mysql_connector) of
        {ok, MysqlConnector} ->
            case lists:keyfind(encoding, 1, MysqlConnector) of
                {encoding, Encoding} ->
                    Encoding;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec cookie() -> atom().
cookie() ->
    cookie(erlang).

-spec cookie(Default :: atom()) -> atom().
cookie(Default) ->
    case application:get_env(main, cookie) of
        {ok, Cookie} ->
            Cookie;
        _ ->
            Default
    end.

-spec node_type() -> atom().
node_type() ->
    node_type(local).

-spec node_type(Default :: atom()) -> atom().
node_type(Default) ->
    case application:get_env(main, node_type) of
        {ok, NodeType} ->
            NodeType;
        _ ->
            Default
    end.

-spec server_id() -> integer().
server_id() ->
    server_id(1000).

-spec server_id(Default :: integer()) -> integer().
server_id(Default) ->
    case application:get_env(main, server_id) of
        {ok, ServerId} ->
            ServerId;
        _ ->
            Default
    end.

-spec server_id_list() -> string().
server_id_list() ->
    server_id_list([]).

-spec server_id_list(Default :: string()) -> string().
server_id_list(Default) ->
    case application:get_env(main, server_id_list) of
        {ok, ServerIdList} ->
            ServerIdList;
        _ ->
            Default
    end.

-spec open_time() -> integer().
open_time() ->
    open_time(1577808000).

-spec open_time(Default :: integer()) -> integer().
open_time(Default) ->
    case application:get_env(main, open_time) of
        {ok, OpenTime} ->
            OpenTime;
        _ ->
            Default
    end.

-spec center_node() -> atom().
center_node() ->
    center_node(center).

-spec center_node(Default :: atom()) -> atom().
center_node(Default) ->
    case application:get_env(main, center_node) of
        {ok, CenterNode} ->
            CenterNode;
        _ ->
            Default
    end.

-spec center_ip() -> string().
center_ip() ->
    center_ip([]).

-spec center_ip(Default :: string()) -> string().
center_ip(Default) ->
    case application:get_env(main, center_ip) of
        {ok, CenterIp} ->
            CenterIp;
        _ ->
            Default
    end.

-spec world_node() -> atom().
world_node() ->
    world_node(world).

-spec world_node(Default :: atom()) -> atom().
world_node(Default) ->
    case application:get_env(main, world_node) of
        {ok, WorldNode} ->
            WorldNode;
        _ ->
            Default
    end.

-spec world_ip() -> string().
world_ip() ->
    world_ip([]).

-spec world_ip(Default :: string()) -> string().
world_ip(Default) ->
    case application:get_env(main, world_ip) of
        {ok, WorldIp} ->
            WorldIp;
        _ ->
            Default
    end.

-spec log_retain_file() -> atom().
log_retain_file() ->
    log_retain_file(false).

-spec log_retain_file(Default :: atom()) -> atom().
log_retain_file(Default) ->
    case application:get_env(main, log_retain_file) of
        {ok, LogRetainFile} ->
            LogRetainFile;
        _ ->
            Default
    end.

-spec path() -> proplists:proplist().
path() ->
    path([{app,"config/app/"},{beam,"beam/"},{config,"config/"},{include,"include/"},{script,"script/"},{src,"src/"}]).

-spec path(Default :: proplists:proplist()) -> proplists:proplist().
path(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            Path;
        _ ->
            Default
    end.

-spec path_app() -> string().
path_app() ->
    path_app("config/app/").

-spec path_app(Default :: string()) -> string().
path_app(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(app, 1, Path) of
                {app, App} ->
                    App;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_beam() -> string().
path_beam() ->
    path_beam("beam/").

-spec path_beam(Default :: string()) -> string().
path_beam(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(beam, 1, Path) of
                {beam, Beam} ->
                    Beam;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_config() -> string().
path_config() ->
    path_config("config/").

-spec path_config(Default :: string()) -> string().
path_config(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(config, 1, Path) of
                {config, Config} ->
                    Config;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_include() -> string().
path_include() ->
    path_include("include/").

-spec path_include(Default :: string()) -> string().
path_include(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(include, 1, Path) of
                {include, Include} ->
                    Include;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_script() -> string().
path_script() ->
    path_script("script/").

-spec path_script(Default :: string()) -> string().
path_script(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(script, 1, Path) of
                {script, Script} ->
                    Script;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec path_src() -> string().
path_src() ->
    path_src("src/").

-spec path_src(Default :: string()) -> string().
path_src(Default) ->
    case application:get_env(main, path) of
        {ok, Path} ->
            case lists:keyfind(src, 1, Path) of
                {src, Src} ->
                    Src;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

