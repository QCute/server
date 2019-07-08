-module(role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").

-define(INSERT_ROLE, "INSERT INTO `role` (`account_name`, `name`, `sex`, `level`, `classes`, `item_size`, `bag_size`, `store_size`, `server_id`, `online`) VALUES ('~s', '~s', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_ROLE, "UPDATE `role` SET `sex` = '~w', `level` = '~w', `classes` = '~w', `item_size` = '~w', `bag_size` = '~w', `store_size` = '~w', `server_id` = '~w', `online` = '~w' WHERE `id` = '~w'").
-define(SELECT_ROLE, "SELECT * FROM `role` WHERE `id` = '~w'").
-define(DELETE_ROLE, "DELETE  FROM `role` WHERE `id` = '~w'").

%% @doc insert
insert(Role) ->
    Sql = io_lib:format(?INSERT_ROLE, [
        Role#role.account_name,
        Role#role.name,
        Role#role.sex,
        Role#role.level,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.server_id,
        Role#role.online
    ]),
    sql:insert(Sql).

%% @doc update
update(Role) ->
    Sql = io_lib:format(?UPDATE_ROLE, [
        Role#role.sex,
        Role#role.level,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.server_id,
        Role#role.online,
        Role#role.id
    ]),
    sql:update(Sql).

%% @doc select
select(Id) ->
    Sql = io_lib:format(?SELECT_ROLE, [
        Id
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_ROLE, [
        Id
    ]),
    sql:delete(Sql).

