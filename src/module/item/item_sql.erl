-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").
-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `number`, `expire_time`) VALUES (~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_ITEM, <<"SELECT `item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`, 0 AS `flag` FROM `item` WHERE `item_no` = ~w">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET `role_id` = ~w, `item_id` = ~w, `type` = ~w, `number` = ~w, `expire_time` = ~w WHERE `item_no` = ~w">>).
-define(DELETE_ITEM, <<"DELETE  FROM `item` WHERE `item_no` = ~w">>).
-define(INSERT_UPDATE_ITEM, {<<"INSERT INTO `item` (`item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `item_id` = VALUES(`item_id`), `type` = VALUES(`type`), `number` = VALUES(`number`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`, 0 AS `flag` FROM `item` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `item`.`item_no`, `item`.`role_id`, `item`.`item_id`, `item`.`type`, `item`.`number`, `item`.`expire_time`, IFNULL(`item`.`flag`, 0) AS `flag` FROM `item` WHERE `item`.`role_id` = ~w">>).
-define(DELETE_IN_ITEM_NO, {<<"DELETE  FROM `item` WHERE `item_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Item) ->
    Sql = parser:format(?INSERT_ITEM, [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.number,
        Item#item.expire_time
    ]),
    db:insert(Sql).

%% @doc select
select(ItemNo) ->
    Sql = parser:format(?SELECT_ITEM, [ItemNo]),
    Data = db:select(Sql),
    parser:convert(Data, item).

%% @doc update
update(Item) ->
    Sql = parser:format(?UPDATE_ITEM, [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.number,
        Item#item.expire_time,
        Item#item.item_no
    ]),
    db:update(Sql).

%% @doc delete
delete(ItemNo) ->
    Sql = parser:format(?DELETE_ITEM, [ItemNo]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Item) -> [
        Item#item.item_no,
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.number,
        Item#item.expire_time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_ITEM, #item.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, item).

%% @doc delete
delete_in_item_no(ItemNoList) ->
    F = fun(ItemNo) -> [ItemNo] end,
    Sql = parser:collect(ItemNoList, F, ?DELETE_IN_ITEM_NO),
    db:delete(Sql).

