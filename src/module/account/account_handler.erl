-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, AccountName]) ->
    account:query(State, ServerId, AccountName);

handle(10002, State, [RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]) ->
    account:create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(10003, State, [RoleId, RoleName, ServerId, AccountName]) ->
    account:login(State, RoleId, RoleName, ServerId, AccountName);

handle(10004, State, []) ->
    account:logout(State);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
