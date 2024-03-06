-module(account_handler).
-export([handle/3]).
-export([send_heartbeat/2]).
-export([send_query/3]).
-export([send_create/4]).
-export([send_login/2]).
-export([send_logout/2]).

handle(Client, 10000, {}) ->
    account:heartbeat(Client);

handle(Client, 10001, {ServerId, AccountName}) ->
    account:query(Client, ServerId, AccountName);

handle(Client, 10002, {RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType}) ->
    account:create(Client, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(Client, 10003, {RoleId, RoleName, ServerId, AccountName}) ->
    account:login(Client, RoleId, RoleName, ServerId, AccountName);

handle(Client, 10004, {}) ->
    account:logout(Client);

handle(Client, Protocol, Data) ->
    account:handle_packet(Client, Protocol, Data).

send_heartbeat(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10000, Data),
    sender:send(Client, Binary).

send_query(Client, Result, List) ->
    {ok, Binary} = account_protocol:encode(10001, {Result, List}),
    sender:send(Client, Binary).

send_create(Client, Result, RoleId, RoleName) ->
    {ok, Binary} = account_protocol:encode(10002, {Result, RoleId, RoleName}),
    sender:send(Client, Binary).

send_login(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10003, Data),
    sender:send(Client, Binary).

send_logout(Client, Data) ->
    {ok, Binary} = account_protocol:encode(10004, Data),
    sender:send(Client, Binary).

