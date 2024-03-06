-module(shop_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_buy/2]).
-include("user.hrl").

handle(User, 11301, []) ->
    shop:query(User);

handle(User, 11302, [ShopId, Number]) ->
    shop:buy(User, ShopId, Number);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = shop_protocol:encode(11301, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_buy(User, Result) ->
    {ok, Binary} = shop_protocol:encode(11302, Result),
    User#user{buffer = [Binary | User#user.buffer]}.

