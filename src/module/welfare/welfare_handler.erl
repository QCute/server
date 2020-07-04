-module(welfare_handler).
-export([handle/3]).

handle(15001, User, []) ->
    sign:award(User);

handle(15002, User, Key) ->
    key_server:award(User, Key);

handle(15003, _, []) ->
    lucky_money_server:query();

handle(15004, User, LuckyMoneyId) ->
    lucky_money_server:receive_lucky_money(User, LuckyMoneyId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
