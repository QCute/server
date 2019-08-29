-module(shop_handler).
-export([handle/3]).

handle(11301, User, []) ->
    shop:push(User);

handle(11302, User, [ShopId, Amount]) ->
    shop:buy(User, ShopId, Amount);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
