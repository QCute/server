-module(shop_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_buy/2]).
-include("user.hrl").

handle(User, 11301, Data) ->
    shop:query(User, Data);

handle(User, 11302, Data) ->
    shop:buy(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = shop_protocol:encode(11301, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_buy(User, Data) ->
    {ok, Binary} = shop_protocol:encode(11302, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

