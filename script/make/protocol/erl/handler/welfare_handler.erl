-module(welfare_handler).
-export([handle/3]).
-export([send_sign/2]).
-export([send_award/2]).
-export([send_query_lucky_money/2]).
-export([send_receive_lucky_money/2]).
-include("user.hrl").

handle(User, 15001, Data) ->
    sign:sign(User, Data);

handle(User, 15002, Data) ->
    key_server:award(User, Data);

handle(User, 15003, Data) ->
    lucky_money_server:query(User, Data);

handle(User, 15004, Data) ->
    lucky_money_server:receive_lucky_money(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_sign(User, Data) ->
    {ok, Binary} = welfare_protocol:encode(15001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award(User, Data) ->
    {ok, Binary} = welfare_protocol:encode(15002, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_lucky_money(User, LuckyMoney) ->
    {ok, Binary} = welfare_protocol:encode(15003, LuckyMoney),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_receive_lucky_money(User, Data) ->
    {ok, Binary} = welfare_protocol:encode(15004, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

