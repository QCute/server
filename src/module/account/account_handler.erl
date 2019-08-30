-module(account_handler).
-export([handle/3]).

handle(10000, State, []) ->
    account:heartbeat(State);

handle(10001, State, [ServerId, AccountName]) ->
    account:login(State, ServerId, AccountName);

handle(10002, State, [ServerId, Sex, Career, ChannelId, Name, Nick, DeviceId, Mac, DeviceType]) ->
    account:create(State, ServerId, Sex, Career, ChannelId, Name, Nick, DeviceId, Mac, DeviceType);

handle(10003, State, [Name]) ->
    account:query(State, Name);

handle(_, State, Data) ->
    account:handle_packet(State, Data).
