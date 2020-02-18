-module(vip_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").
-define(INSERT_VIP, <<"INSERT INTO `vip` (`role_id`, `vip_level`, `exp`, `expire_time`) VALUES (~w, ~w, ~w, ~w)">>).
-define(SELECT_VIP, <<"SELECT `role_id`, `vip_level`, `exp`, `expire_time` FROM `vip` WHERE `role_id` = ~w">>).
-define(UPDATE_VIP, <<"UPDATE `vip` SET `vip_level` = ~w, `exp` = ~w, `expire_time` = ~w WHERE `role_id` = ~w">>).
-define(DELETE_VIP, <<"DELETE  FROM `vip` WHERE `role_id` = ~w">>).
-define(TRUNCATE, <<"TRUNCATE TABLE `vip`">>).

%% @doc insert
insert(Vip) ->
    Sql = parser:format(?INSERT_VIP, [
        Vip#vip.role_id,
        Vip#vip.vip_level,
        Vip#vip.exp,
        Vip#vip.expire_time
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_VIP, [RoleId]),
    Data = sql:select(Sql),
    parser:convert(Data, vip).

%% @doc update
update(Vip) ->
    Sql = parser:format(?UPDATE_VIP, [
        Vip#vip.vip_level,
        Vip#vip.exp,
        Vip#vip.expire_time,
        Vip#vip.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_VIP, [RoleId]),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

