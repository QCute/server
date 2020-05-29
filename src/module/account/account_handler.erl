-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, Account]) ->
    account:query(State, ServerId, Account);

handle(10002, State, [ServerId, Account, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]) ->
    account:create(State, ServerId, Account, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(10003, State, [ServerId, Account]) ->
    account:login(State, ServerId, Account);

handle(10004, State, [ServerId, Account]) ->
    account:logout(State, ServerId, Account);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
