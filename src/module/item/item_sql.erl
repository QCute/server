-module(item_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-export([delete_in_item_no/1]).
-include("item.hrl").

-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `number`, `expire_time`) VALUES (~i~i~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_ITEM, <<"SELECT `item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`, 0 AS `flag` FROM `item` WHERE `item_no` = ~w">>).
-define(UPDATE_ITEM, {<<"UPDATE `item` SET ~i~i`role_id` = ~w, `item_id` = ~w, `type` = ~w, `number` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `item_no` = ~w">>}).
-define(DELETE_ITEM, <<"DELETE FROM `item` WHERE `item_no` = ~w">>).
-define(INSERT_UPDATE_ITEM, {<<"INSERT INTO `item` (`item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `item_id` = VALUES(`item_id`), `type` = VALUES(`type`), `number` = VALUES(`number`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`, 0 AS `flag` FROM `item` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `item`.`item_no`, `item`.`role_id`, `item`.`item_id`, `item`.`type`, `item`.`number`, `item`.`expire_time`, IFNULL(`item`.`flag`, 0) AS `flag` FROM `item` WHERE `item`.`role_id` = ~w">>).
-define(DELETE_IN_ITEM_NO, {<<"DELETE FROM `item` WHERE `item_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Item :: #item{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Item) ->
    Sql = parser:format(?INSERT_ITEM, Item),
    db:insert(Sql).

%% @doc select
-spec select(ItemNo :: integer()) -> ItemList :: [#item{}].
select(ItemNo) ->
    Sql = parser:format(?SELECT_ITEM, [ItemNo]),
    Data = db:select(Sql),
    parser:convert(Data, item).

%% @doc update
-spec update(Item :: #item{}) -> AffectedRows :: non_neg_integer().
update(Item) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ITEM), Item))/binary, (parser:format(element(2, ?UPDATE_ITEM), [Item#item.item_no]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(ItemNo :: integer()) -> AffectedRows :: non_neg_integer().
delete(ItemNo) ->
    Sql = parser:format(?DELETE_ITEM, [ItemNo]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(ItemList :: [#item{}] | ets:tab()) -> NewItemList :: [#item{}].
insert_update(ItemList) ->
    {Sql, NewItemList} = parser:collect_into(ItemList, ?INSERT_UPDATE_ITEM, #item.flag),
    db:insert(Sql),
    NewItemList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> ItemList :: [#item{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, item).

%% @doc delete
-spec delete_in_item_no(ItemNoList :: [ItemNo :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_item_no(ItemNoList) ->
    Sql = parser:collect(ItemNoList, ?DELETE_IN_ITEM_NO),
    db:delete(Sql).

