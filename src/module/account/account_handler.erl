-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, Account]) ->
    account:login(State, ServerId, Account);

handle(10002, State, [Account, RoleName, ServerId, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType]) ->
    account:create(State, Account, RoleName, ServerId, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
