-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").

-define(UPDATE_INTO_ITEM, {"INSERT INTO `item` (`id`, `role_id`, `data_id`, `type`, `amount`, `bind`) VALUES ", "('~w', '~w', '~w', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `amount` = VALUES(`amount`), `bind` = VALUES(`bind`)"}).
-define(INSERT_ITEM, "INSERT INTO `item` (`role_id`, `data_id`, `type`, `amount`, `bind`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_ITEM, "UPDATE `item` SET `type` = '~w', `amount` = '~w', `bind` = '~w' WHERE `id` = '~w'").
-define(SELECT_ITEM, "SELECT * FROM `item` WHERE `role_id` = '~w'").
-define(DELETE_ITEM, "DELETE  FROM `item` WHERE `id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Item) -> [
        Item#item.item_id,
        Item#item.role_id,
        Item#item.data_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_ITEM, #item.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Item) ->
    Sql = io_lib:format(?INSERT_ITEM, [
        Item#item.role_id,
        Item#item.data_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ]),
    sql:insert(Sql).

%% @doc update
update(Item) ->
    Sql = io_lib:format(?UPDATE_ITEM, [
        Item#item.type,
        Item#item.amount,
        Item#item.bind,
        Item#item.item_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_ITEM, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_ITEM, [
        Id
    ]),
    sql:delete(Sql).

