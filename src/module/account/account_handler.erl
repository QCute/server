-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, AccountName]) ->
    account:query(State, ServerId, AccountName);

handle(10002, State, [ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]) ->
    account:create(State, ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(10003, State, [ServerId, AccountName]) ->
    account:login(State, ServerId, AccountName);

handle(10004, State, []) ->
    account:logout(State);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
