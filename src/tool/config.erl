-module(config).
-compile(nowarn_export_all).
-compile(export_all).


host() ->
    case application:get_env(pool, host) of
        {ok, Value} ->
            Value;
        _ ->
            "127.0.0.1"
    end.

port() ->
    case application:get_env(pool, port) of
        {ok, Value} ->
            Value;
        _ ->
            3306
    end.

user() ->
    case application:get_env(pool, user) of
        {ok, Value} ->
            Value;
        _ ->
            "root"
    end.

password() ->
    case application:get_env(pool, password) of
        {ok, Value} ->
            Value;
        _ ->
            "root"
    end.

database() ->
    case application:get_env(pool, database) of
        {ok, Value} ->
            Value;
        _ ->
            "main"
    end.

encode() ->
    case application:get_env(pool, encode) of
        {ok, Value} ->
            Value;
        _ ->
            utf8mb4
    end.

ssl_file() ->
    case application:get_env(main, ssl_file) of
        {ok, Value} ->
            Value;
        _ ->
            "fake"
    end.

server_no() ->
    case application:get_env(main, server_no) of
        {ok, Value} ->
            Value;
        _ ->
            0
    end.

gen_tcp_port() ->
    case application:get_env(main, gen_tcp_port) of
        {ok, Value} ->
            Value;
        _ ->
            "8000"
    end.

ssl_port() ->
    case application:get_env(main, ssl_port) of
        {ok, Value} ->
            Value;
        _ ->
            "9000"
    end.

socket_type() ->
    case application:get_env(main, socket_type) of
        {ok, Value} ->
            Value;
        _ ->
            gen_tcp
    end.

node_prefix() ->
    case application:get_env(main, node_prefix) of
        {ok, Value} ->
            Value;
        _ ->
            "erlang"
    end.

game_version() ->
    case application:get_env(main, game_version) of
        {ok, Value} ->
            Value;
        _ ->
            1
    end.

node_type() ->
    case application:get_env(main, node_type) of
        {ok, Value} ->
            Value;
        _ ->
            local
    end.

include() ->
    case application:get_env(main, include) of
        {ok, Value} ->
            Value;
        _ ->
            "include/"
    end.

beam() ->
    case application:get_env(main, beam) of
        {ok, Value} ->
            Value;
        _ ->
            "beam/"
    end.

src() ->
    case application:get_env(main, src) of
        {ok, Value} ->
            Value;
        _ ->
            "src/"
    end.

