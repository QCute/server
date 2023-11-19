-module(shop_sql).
-export([save/1]).
-export([select/1]).
-include("shop.hrl").

%% @doc insert into shop
-spec save(ShopList :: [#shop{}] | ets:tab()) -> NewShopList :: [#shop{}].
save(ShopList) ->
    db:save_into(<<"INSERT INTO `shop` (`role_id`, `shop_id`, `number`) VALUES">>, <<"(:2:, :3:, :4:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `shop_id` = VALUES(`shop_id`), `number` = VALUES(`number`)">>, ShopList, #shop.flag).

%% @doc select from shop
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#shop{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `shop_id`, `number`, `flag` FROM `shop` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, shop).
