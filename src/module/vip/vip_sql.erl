-module(vip_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").

-define(INSERT_VIP, "INSERT INTO `vip` (`role_id`, `level`, `exp`, `expire_time`) VALUES ('~w', '~w', '~w', '~w')").
-define(UPDATE_VIP, "UPDATE `vip` SET `level` = '~w', `exp` = '~w', `expire_time` = '~w' WHERE `role_id` = '~w'").
-define(SELECT_VIP, "SELECT * FROM `vip` WHERE `role_id` = '~w'").
-define(DELETE_VIP, "DELETE  FROM `vip` WHERE `role_id` = '~w'").

%% @doc insert
insert(Vip) ->
    Sql = io_lib:format(?INSERT_VIP, [
        Vip#vip.role_id,
        Vip#vip.level,
        Vip#vip.exp,
        Vip#vip.expire_time
    ]),
    sql:insert(Sql).

%% @doc update
update(Vip) ->
    Sql = io_lib:format(?UPDATE_VIP, [
        Vip#vip.level,
        Vip#vip.exp,
        Vip#vip.expire_time,
        Vip#vip.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_VIP, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = io_lib:format(?DELETE_VIP, [
        RoleId
    ]),
    sql:delete(Sql).

