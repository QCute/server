-module(welfare_handler).
-export([handle/3]).

handle(15001, User, []) ->
    sign:sign(User);

handle(15002, User, Key) ->
    key_server:award(User, Key);

handle(15003, _, LuckyMoneyNo) ->
    lucky_money_server:query(LuckyMoneyNo);

handle(15004, User, LuckyMoneyNo) ->
    lucky_money_server:receive_lucky_money(User, LuckyMoneyNo);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
