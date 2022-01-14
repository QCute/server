-module(shop_handler).
-export([handle/3]).

handle(User, 11301, []) ->
    shop:query(User);

handle(User, 11302, [ShopId, Number]) ->
    shop:buy(User, ShopId, Number);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
