-module(item_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("item.hrl").

-define(INSERT_ITEM, <<"INSERT INTO `item` (`user_id`, `base_id`, `amount`) VALUES ('~w', '~w', '~w')">>).
-define(UPDATE_ITEM, <<"UPDATE `item` SET (`amount`) VALUES ('~w') WHERE `id` = '~w'">>).
-define(SELECT_ITEM, <<"SELECT * FROM `item` WHERE `user_id` = '~w'">>).
-define(DELETE_ITEM, <<"DELETE * FROM `item` WHERE `id` = '~w'">>).
%% @doc insert
insert(Item) ->
    Sql = io_lib:format(?INSERT_ITEM, [
        Item#item.user_id,
        Item#item.base_id,
        Item#item.amount
    ]),
    sql:execute(?DB_GAME, item, Sql).


%% @doc update
update(Item) ->
    Sql = io_lib:format(?UPDATE_ITEM, [
        Item#item.amount,
        Item#item.id
    ]),
    sql:execute(?DB_GAME, item, Sql).

%% @doc select
select(UserId) ->
    Sql = io_lib:format(?SELECT_ITEM, [
        UserId
    ]),
    sql:execute(?DB_GAME, item, Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_ITEM, [
        Id
    ]),
    sql:execute(?DB_GAME, item, Sql).
