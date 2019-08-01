-module(role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").

-define(INSERT_ROLE, <<"INSERT INTO `role` (`role_name`, `account_id`, `account_name`, `sex`, `level`, `classes`, `item_size`, `bag_size`, `store_size`, `server_id`, `online`) VALUES ('~s', '~s', '~s', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w')">>).
-define(UPDATE_ROLE, <<"UPDATE `role` SET `sex` = '~w', `level` = '~w', `classes` = '~w', `item_size` = '~w', `bag_size` = '~w', `store_size` = '~w', `server_id` = '~w', `online` = '~w' WHERE `role_id` = '~w'">>).
-define(SELECT_ROLE, <<"SELECT * FROM `role` WHERE `role_id` = '~w'">>).
-define(DELETE_ROLE, <<"DELETE  FROM `role` WHERE `role_id` = '~w'">>).

%% @doc insert
insert(Role) ->
    Sql = parser:format(?INSERT_ROLE, [
        Role#role.role_name,
        Role#role.account_id,
        Role#role.account_name,
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
    Sql = parser:format(?UPDATE_ROLE, [
        Role#role.sex,
        Role#role.level,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.server_id,
        Role#role.online,
        Role#role.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ROLE, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ROLE, [
        RoleId
    ]),
    sql:delete(Sql).

