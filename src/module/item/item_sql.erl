-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").
-define(INSERT_ITEM, <<"INSERT INTO `item` (`role_id`, `item_id`, `type`, `number`, `bind`, `expire_time`) VALUES (~w, ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_ITEM, <<"SELECT `unique_id`, `role_id`, `item_id`, `type`, `number`, `bind`, `expire_time`, 0 AS `flag` FROM `item` WHERE `role_id` = ~w">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET `type` = ~w, `number` = ~w, `bind` = ~w, `expire_time` = ~w WHERE `unique_id` = ~w">>).
-define(DELETE_ITEM, <<"DELETE  FROM `item` WHERE `unique_id` = ~w">>).
-define(INSERT_UPDATE_ITEM, {<<"INSERT INTO `item` (`unique_id`, `role_id`, `item_id`, `type`, `number`, `bind`, `expire_time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `item_id` = VALUES(`item_id`), `type` = VALUES(`type`), `number` = VALUES(`number`), `bind` = VALUES(`bind`), `expire_time` = VALUES(`expire_time`)">>}).
-define(DELETE_IN_UNIQUE_ID, {<<"DELETE  FROM `item` WHERE `unique_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Item) ->
    Sql = parser:format(?INSERT_ITEM, [
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.number,
        Item#item.bind,
        Item#item.expire_time
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ITEM, [RoleId]),
    Data = sql:select(Sql),
    parser:convert(Data, item).

%% @doc update
update(Item) ->
    Sql = parser:format(?UPDATE_ITEM, [
        Item#item.type,
        Item#item.number,
        Item#item.bind,
        Item#item.expire_time,
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
        Item#item.unique_id,
        Item#item.role_id,
        Item#item.item_id,
        Item#item.type,
        Item#item.number,
        Item#item.bind,
        Item#item.expire_time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_ITEM, #item.flag),
    sql:insert(Sql),
    NewData.

%% @doc delete
delete_in_unique_id(UniqueIdList) ->
    F = fun(UniqueId) -> [UniqueId] end,
    Sql = parser:collect(UniqueIdList, F, ?DELETE_IN_UNIQUE_ID),
    sql:delete(Sql).

