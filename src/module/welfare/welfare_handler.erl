-module(welfare_handler).
-export([handle/3]).

handle(User, 15001, []) ->
    sign:sign(User);

handle(User, 15002, Key) ->
    key_server:award(User, Key);

handle(_, 15003, LuckyMoneyNo) ->
    lucky_money_server:query(LuckyMoneyNo);

handle(User, 15004, LuckyMoneyNo) ->
    lucky_money_server:receive_lucky_money(User, LuckyMoneyNo);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
