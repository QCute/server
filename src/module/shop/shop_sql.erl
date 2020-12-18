-module(shop_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("shop.hrl").
-define(INSERT_SHOP, <<"INSERT INTO `shop` (`role_id`, `shop_id`, `number`) VALUES (~w, ~w, ~w)">>).
-define(SELECT_SHOP, <<"SELECT `role_id`, `shop_id`, `number`, 0 AS `flag` FROM `shop` WHERE `role_id` = ~w AND `shop_id` = ~w">>).
-define(UPDATE_SHOP, <<"UPDATE `shop` SET `number` = ~w WHERE `role_id` = ~w AND `shop_id` = ~w">>).
-define(DELETE_SHOP, <<"DELETE  FROM `shop` WHERE `role_id` = ~w AND `shop_id` = ~w">>).
-define(INSERT_UPDATE_SHOP, {<<"INSERT INTO `shop` (`role_id`, `shop_id`, `number`) VALUES ">>, <<"(~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `number` = VALUES(`number`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `shop_id`, `number`, 0 AS `flag` FROM `shop` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `shop`.`role_id`, `shop`.`shop_id`, `shop`.`number`, IFNULL(`shop`.`flag`, 0) AS `flag` FROM `shop` WHERE `shop`.`role_id` = ~w">>).

%% @doc insert
insert(Shop) ->
    Sql = parser:format(?INSERT_SHOP, [
        Shop#shop.role_id,
        Shop#shop.shop_id,
        Shop#shop.number
    ]),
    db:insert(Sql).

%% @doc select
select(RoleId, ShopId) ->
    Sql = parser:format(?SELECT_SHOP, [RoleId, ShopId]),
    Data = db:select(Sql),
    parser:convert(Data, shop).

%% @doc update
update(Shop) ->
    Sql = parser:format(?UPDATE_SHOP, [
        Shop#shop.number,
        Shop#shop.role_id,
        Shop#shop.shop_id
    ]),
    db:update(Sql).

%% @doc delete
delete(RoleId, ShopId) ->
    Sql = parser:format(?DELETE_SHOP, [RoleId, ShopId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Shop) -> [
        Shop#shop.role_id,
        Shop#shop.shop_id,
        Shop#shop.number
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_SHOP, #shop.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, shop).

