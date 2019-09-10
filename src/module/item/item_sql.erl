-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").
-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(SELECT_ITEM, <<"SELECT * FROM `item` WHERE `unique_id` = '~w'">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET `type` = '~w', `amount` = '~w', `bind` = '~w' WHERE `unique_id` = '~w'">>).
-define(DELETE_ITEM, <<"DELETE  FROM `item` WHERE `unique_id` = '~w'">>).
-define(INSERT_UPDATE_ITEM, {<<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `amount`, `bind`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `type` = '~w', `amount` = '~w', `bind` = '~w'">>}).
-define(SELECT_JOIN_ITEM, <<"SELECT `item`.`unique_id`, `item`.`role_id`, `item`.`item_id`, `item`.`type`, `item`.`amount`, `item`.`bind`, `item`.`flag` FROM `item` WHERE `item`.`unique_id` = '~w'">>).
-define(DELETE_IN_UNIQUE_ID, {<<"DELETE  FROM `item` WHERE `unique_id` in (">>, <<"'~w'">>, <<")">>}).

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

%% @doc select
select(UniqueId) ->
    Sql = parser:format(?SELECT_ITEM, [UniqueId]),
    sql:select(Sql).

%% @doc update
update(Item) ->
    Sql = parser:format(?UPDATE_ITEM, [
        Item#item.type,
        Item#item.amount,
        Item#item.bind,
        Item#item.unique_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(UniqueId) ->
    Sql = parser:format(?DELETE_ITEM, [UniqueId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Item) -> [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.amount,
        Item#item.bind
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_ITEM, #item.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join(UniqueId) ->
    Sql = parser:format(?SELECT_JOIN_ITEM, [UniqueId]),
    sql:select(Sql).

%% @doc delete
delete_in_unique_id(UniqueIdList) ->
	F = fun(UniqueId) -> [UniqueId] end,
    Sql = parser:collect(UniqueIdList, F, ?DELETE_IN_UNIQUE_ID),
    sql:delete(Sql).

