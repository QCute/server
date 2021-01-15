-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, AccountNameBinary]) ->
    account:query(State, ServerId, AccountNameBinary);

handle(10002, State, [RoleNameBinary, ServerId, AccountNameBinary, Sex, Classes, ChannelBinary, DeviceIdBinary, MacBinary, DeviceTypeBinary]) ->
    account:create(State, RoleNameBinary, ServerId, AccountNameBinary, Sex, Classes, ChannelBinary, DeviceIdBinary, MacBinary, DeviceTypeBinary);

handle(10003, State, [RoleId, RoleNameBinary, ServerId, AccountNameBinary]) ->
    account:login(State, RoleId, RoleNameBinary, ServerId, AccountNameBinary);

handle(10004, State, []) ->
    account:logout(State);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
