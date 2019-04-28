-module(shop_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("shop.hrl").

-define(UPDATE_INTO_SHOP, {"INSERT INTO `shop` (`player_id`, `shop_id`, `amount`) VALUES ", "('~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `amount` = VALUES(`amount`)"}).
-define(INSERT_SHOP, "INSERT INTO `shop` (`player_id`, `shop_id`, `amount`) VALUES ('~w', '~w', '~w')").
-define(UPDATE_SHOP, "UPDATE `shop` SET `amount` = '~w' WHERE `player_id` = '~w' AND `shop_id` = '~w'").
-define(SELECT_SHOP, "SELECT * FROM `shop` WHERE `player_id` = '~w'").
-define(DELETE_SHOP, "DELETE * FROM `shop` WHERE `player_id` = '~w' AND `shop_id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Shop) -> [
        Shop#shop.player_id,
        Shop#shop.shop_id,
        Shop#shop.amount
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_SHOP, #shop.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Shop) ->
    Sql = io_lib:format(?INSERT_SHOP, [
        Shop#shop.player_id,
        Shop#shop.shop_id,
        Shop#shop.amount
    ]),
    sql:insert(Sql).

%% @doc update
update(Shop) ->
    Sql = io_lib:format(?UPDATE_SHOP, [
        Shop#shop.amount,
        Shop#shop.player_id,
        Shop#shop.shop_id
    ]),
    sql:update(Sql).

%% @doc select
select(PlayerId) ->
    Sql = io_lib:format(?SELECT_SHOP, [
        PlayerId
    ]),
    sql:select(Sql).

%% @doc delete
delete(PlayerId, ShopId) ->
    Sql = io_lib:format(?DELETE_SHOP, [
        PlayerId,
        ShopId
    ]),
    sql:delete(Sql).

