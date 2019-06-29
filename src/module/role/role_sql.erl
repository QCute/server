-module(role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").

-define(INSERT_role, "INSERT INTO `role` (`account`, `name`, `sex`, `level`, `classes`, `item_size`, `bag_size`, `store_size`, `focus`) VALUES ('~s', '~s', '~w', '~w', '~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_role, "UPDATE `role` SET `sex` = '~w', `level` = '~w', `classes` = '~w', `item_size` = '~w', `bag_size` = '~w', `store_size` = '~w', `focus` = '~w' WHERE `id` = '~w'").
-define(SELECT_role, "SELECT * FROM `role` WHERE `id` = '~w'").
-define(DELETE_role, "DELETE  FROM `role` WHERE `id` = '~w'").

%% @doc insert
insert(Role) ->
    Sql = io_lib:format(?INSERT_role, [
        Role#role.account,
        Role#role.name,
        Role#role.sex,
        Role#role.level,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.focus
    ]),
    sql:insert(Sql).

%% @doc update
update(Role) ->
    Sql = io_lib:format(?UPDATE_role, [
        Role#role.sex,
        Role#role.level,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.focus,
        Role#role.id
    ]),
    sql:update(Sql).

%% @doc select
select(Id) ->
    Sql = io_lib:format(?SELECT_role, [
        Id
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_role, [
        Id
    ]),
    sql:delete(Sql).

