-module(shop_handler).
-export([handle/3]).

handle(11301, User, []) ->
    shop:query(User);

handle(11302, User, [ShopId, Number]) ->
    shop:buy(User, ShopId, Number);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
