-module(welfare_handler).
-export([handle/3]).
-export([send_sign/2]).
-export([send_award/2]).
-export([send_query_lucky_money/2]).
-export([send_receive_lucky_money/3]).
-include("user.hrl").

handle(User, 15001, []) ->
    sign:sign(User);

handle(User, 15002, Key) ->
    key_server:award(User, Key);

handle(User, 15003, LuckyMoneyNo) ->
    lucky_money_server:query(User, LuckyMoneyNo);

handle(User, 15004, LuckyMoneyNo) ->
    lucky_money_server:receive_lucky_money(User, LuckyMoneyNo);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_sign(User, Result) ->
    {ok, Binary} = welfare_protocol:encode(15001, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award(User, Result) ->
    {ok, Binary} = welfare_protocol:encode(15002, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_lucky_money(User, LuckyMoney) ->
    {ok, Binary} = welfare_protocol:encode(15003, LuckyMoney),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_receive_lucky_money(User, Result, Gold) ->
    {ok, Binary} = welfare_protocol:encode(15004, [Result, Gold]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

