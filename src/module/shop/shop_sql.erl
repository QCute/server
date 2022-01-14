-module(shop_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("shop.hrl").

-define(INSERT_SHOP, <<"INSERT INTO `shop` (`role_id`, `shop_id`, `number`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_SHOP, <<"SELECT `role_id`, `shop_id`, `number`, 0 AS `flag` FROM `shop` WHERE `role_id` = ~w AND `shop_id` = ~w">>).
-define(UPDATE_SHOP, {<<"UPDATE `shop` SET ~i~i~i`number` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `shop_id` = ~w">>}).
-define(DELETE_SHOP, <<"DELETE FROM `shop` WHERE `role_id` = ~w AND `shop_id` = ~w">>).
-define(INSERT_UPDATE_SHOP, {<<"INSERT INTO `shop` (`role_id`, `shop_id`, `number`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `number` = VALUES(`number`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `shop_id`, `number`, 0 AS `flag` FROM `shop` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `shop`.`role_id`, `shop`.`shop_id`, `shop`.`number`, IFNULL(`shop`.`flag`, 0) AS `flag` FROM `shop` WHERE `shop`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Shop :: #shop{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Shop) ->
    Sql = parser:format(?INSERT_SHOP, Shop),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), ShopId :: integer()) -> ShopList :: [#shop{}].
select(RoleId, ShopId) ->
    Sql = parser:format(?SELECT_SHOP, [RoleId, ShopId]),
    Data = db:select(Sql),
    parser:convert(Data, shop).

%% @doc update
-spec update(Shop :: #shop{}) -> AffectedRows :: non_neg_integer().
update(Shop) ->
    Sql = <<(parser:format(element(1, ?UPDATE_SHOP), Shop))/binary, (parser:format(element(2, ?UPDATE_SHOP), [Shop#shop.role_id, Shop#shop.shop_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), ShopId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, ShopId) ->
    Sql = parser:format(?DELETE_SHOP, [RoleId, ShopId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(ShopList :: [#shop{}] | ets:tab()) -> NewShopList :: [#shop{}].
insert_update(ShopList) ->
    {Sql, NewShopList} = parser:collect_into(ShopList, ?INSERT_UPDATE_SHOP, #shop.flag),
    db:insert(Sql),
    NewShopList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> ShopList :: [#shop{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, shop).

