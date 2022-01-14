-module(account_handler).
-export([handle/3]).

handle(State, 10000, []) ->
    account:heartbeat(State);

handle(State, 10001, [ServerId, AccountName]) ->
    account:query(State, ServerId, AccountName);

handle(State, 10002, [RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]) ->
    account:create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(State, 10003, [RoleId, RoleName, ServerId, AccountName]) ->
    account:login(State, RoleId, RoleName, ServerId, AccountName);

handle(State, 10004, []) ->
    account:logout(State);

handle(State, Protocol, Data) ->
    account:handle_packet(State, Protocol, Data).
