-module(buff_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").
-define(INSERT_BUFF, <<"INSERT INTO `buff` (`role_id`, `buff_id`, `expire_time`, `overlap`) VALUES (~w, ~w, ~w, ~w)">>).
-define(SELECT_BUFF, <<"SELECT `role_id`, `buff_id`, `expire_time`, `overlap`, 0 AS `flag` FROM `buff` WHERE `role_id` = ~w AND `buff_id` = ~w">>).
-define(UPDATE_BUFF, <<"UPDATE `buff` SET `expire_time` = ~w, `overlap` = ~w WHERE `role_id` = ~w AND `buff_id` = ~w">>).
-define(DELETE_BUFF, <<"DELETE  FROM `buff` WHERE `role_id` = ~w AND `buff_id` = ~w">>).
-define(INSERT_UPDATE_BUFF, {<<"INSERT INTO `buff` (`role_id`, `buff_id`, `expire_time`, `overlap`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `expire_time` = VALUES(`expire_time`), `overlap` = VALUES(`overlap`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `buff_id`, `expire_time`, `overlap`, 0 AS `flag` FROM `buff` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `buff`.`role_id`, `buff`.`buff_id`, `buff`.`expire_time`, `buff`.`overlap`, IFNULL(`buff`.`flag`, 0) AS `flag` FROM `buff` WHERE `buff`.`role_id` = ~w">>).

%% @doc insert
insert(Buff) ->
    Sql = parser:format(?INSERT_BUFF, [
        Buff#buff.role_id,
        Buff#buff.buff_id,
        Buff#buff.expire_time,
        Buff#buff.overlap
    ]),
    db:insert(Sql).

%% @doc select
select(RoleId, BuffId) ->
    Sql = parser:format(?SELECT_BUFF, [RoleId, BuffId]),
    Data = db:select(Sql),
    parser:convert(Data, buff).

%% @doc update
update(Buff) ->
    Sql = parser:format(?UPDATE_BUFF, [
        Buff#buff.expire_time,
        Buff#buff.overlap,
        Buff#buff.role_id,
        Buff#buff.buff_id
    ]),
    db:update(Sql).

%% @doc delete
delete(RoleId, BuffId) ->
    Sql = parser:format(?DELETE_BUFF, [RoleId, BuffId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Buff) -> [
        Buff#buff.role_id,
        Buff#buff.buff_id,
        Buff#buff.expire_time,
        Buff#buff.overlap
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_BUFF, #buff.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, buff).

