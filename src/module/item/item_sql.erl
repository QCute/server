-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").
-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(DELETE_ITEM, <<"DELETE  FROM `item` WHERE `unique_id` = '~w'">>).
-define(SELECT_ITEM, <<"SELECT * FROM `item` WHERE `role_id` = '~w'">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET `type` = '~w', `amount` = '~w', `bind` = '~w' WHERE `unique_id` = '~w'">>).
-define(INSERT_UPDATE_ITEM, {<<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `type` = '~w', `amount` = '~w', `bind` = '~w'">>}).

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

%% @doc delete
delete(UniqueId) ->
    Sql = parser:format(?DELETE_ITEM, [UniqueId]),
    sql:delete(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ITEM, [RoleId]),
    sql:select(Sql).

%% @doc update
update(Item) ->
    Sql = parser:format(?UPDATE_ITEM, [
        Item#item.unique_id
    ]),
    sql:update(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Item) -> [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ] end,
    {Sql, NewData} = parser:collect(Data, F, ?INSERT_UPDATE_ITEM, flag),
    sql:insert(Sql),
    NewData.

