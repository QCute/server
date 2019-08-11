-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").

-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET `type` = '~w', `amount` = '~w', `bind` = '~w' WHERE `unique_id` = '~w'">>).
-define(SELECT_ITEM, <<"SELECT * FROM `item` WHERE `role_id` = '~w'">>).
-define(DELETE_ITEM, <<"DELETE  FROM `item` WHERE `unique_id` = '~w'">>).
-define(UPDATE_INTO_ITEM, {<<"INSERT INTO `item` (`unique_id`, `role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `amount` = VALUES(`amount`), `bind` = VALUES(`bind`)">>}).

%% @doc update_into
update_into(DataList) ->
    F = fun(Item) -> [
        Item#item.unique_id,
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_ITEM, #item.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Item) ->
    Sql = parser:format(?INSERT_ITEM, [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ]),
    sql:insert(Sql).

%% @doc update
update(Item) ->
    Sql = parser:format(?UPDATE_ITEM, [
        Item#item.type,
        Item#item.amount,
        Item#item.bind,
        Item#item.unique_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ITEM, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(UniqueId) ->
    Sql = parser:format(?DELETE_ITEM, [
        UniqueId
    ]),
    sql:delete(Sql).

