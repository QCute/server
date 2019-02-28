-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("item.hrl").

-define(UPDATE_INTO_ITEM, {"INSERT INTO `item` (`id`, `user_id`, `data_id`, `amount`, `bind`) VALUES ", "('~w', '~w', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `amount` = VALUES(`amount`), `bind` = VALUES(`bind`)"}).
-define(INSERT_ITEM, "INSERT INTO `item` (`user_id`, `data_id`, `amount`, `bind`) VALUES ('~w', '~w', '~w', '~w')").
-define(UPDATE_ITEM, "UPDATE `item` SET `amount` = '~w', `bind` = '~w' WHERE `id` = '~w'").
-define(SELECT_ITEM, "SELECT * FROM `item` WHERE `user_id` = '~w'").
-define(DELETE_ITEM, "DELETE * FROM `item` WHERE `id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Item) -> [
        Item#item.id,
        Item#item.user_id,
        Item#item.data_id,
        Item#item.amount,
        Item#item.bind
    ] end,
    {Sql, NewData} = data_tool:collect(DataList, F, ?UPDATE_INTO_ITEM, #item.extra),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Item) ->
    Sql = io_lib:format(?INSERT_ITEM, [
        Item#item.user_id,
        Item#item.data_id,
        Item#item.amount,
        Item#item.bind
    ]),
    sql:insert(Sql).

%% @doc update
update(Item) ->
    Sql = io_lib:format(?UPDATE_ITEM, [
        Item#item.amount,
        Item#item.bind,
        Item#item.id
    ]),
    sql:update(Sql).

%% @doc select
select(UserId) ->
    Sql = io_lib:format(?SELECT_ITEM, [
        UserId
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_ITEM, [
        Id
    ]),
    sql:delete(Sql).

