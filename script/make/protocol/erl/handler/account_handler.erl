-module(account_handler).
-export([handle/3]).
-export([send_heartbeat/2]).
-export([send_query/2]).
-export([send_create/2]).
-export([send_login/2]).
-export([send_logout/2]).

handle(Client, 10000, Data) ->
    account:heartbeat(Client, Data);

handle(Client, 10001, Data) ->
    account:query(Client, Data);

handle(Client, 10002, Data) ->
    account:create(Client, Data);

handle(Client, 10003, Data) ->
    account:login(Client, Data);

handle(Client, 10004, Data) ->
    account:logout(Client, Data);

handle(Client, Protocol, Data) ->
    account:handle_packet(Client, Protocol, Data).

send_heartbeat(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10000, Data),
    sender:send(Client, Binary).

send_query(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10001, Data),
    sender:send(Client, Binary).

send_create(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10002, Data),
    sender:send(Client, Binary).

send_login(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10003, Data),
    sender:send(Client, Binary).

send_logout(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10004, Data),
    sender:send(Client, Binary).

