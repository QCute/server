-module(shop_data).
-export([get/1]).
-include("shop.hrl").

-spec get(ShopId :: non_neg_integer()) -> #shop_data{}.
get(1) ->
    #shop_data{shop_id = 1, item_id = 1, type = 1, pay_asset = gold, price = 10, number = 1, level = 0, limit = 10, vip_level = 0, vip_limit = [], description = <<""/utf8>>};
get(_) ->
    undefined.

