-module(shop_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("shop.hrl").


get(1) ->
    #shop_data{shop_id = 1, item_id = 1, type = 1, pay_assets = gold, price = 10, number = 1, level = 0, limit = 0, vip_level = 0, vip_limit = [], description = <<""/utf8>>};
get(_) ->
    [].


