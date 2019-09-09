-module(buff_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").
-define(INSERT_BUFF, <<"INSERT INTO `buff` (`role_id`, `buff_id`, `start_time`, `end_time`, `overlap`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(SELECT_BUFF, <<"SELECT * FROM `buff` WHERE `role_id` = '~w'">>).
-define(UPDATE_BUFF, <<"UPDATE `buff` SET `start_time` = '~w', `end_time` = '~w', `overlap` = '~w' WHERE `role_id` = '~w' AND `buff_id` = '~w'">>).
-define(DELETE_BUFF, <<"DELETE  FROM `buff` WHERE `role_id` = '~w' AND `buff_id` = '~w'">>).
-define(INSERT_UPDATE_BUFF, {<<"INSERT INTO `buff` (`role_id`, `buff_id`, `start_time`, `end_time`, `overlap`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `start_time` = '~w', `end_time` = '~w', `overlap` = '~w'">>}).
-define(SELECT_JOIN_BUFF, <<"SELECT `buff`.`role_id`, `buff`.`buff_id`, `buff`.`start_time`, `buff`.`end_time`, `buff`.`overlap`, `buff`.`flag` FROM `buff` WHERE `buff`.`role_id` = '~w'">>).

%% @doc insert
insert(Buff) ->
    Sql = parser:format(?INSERT_BUFF, [
        Buff#buff.role_id,
        Buff#buff.buff_id,
        Buff#buff.start_time,
        Buff#buff.end_time,
        Buff#buff.overlap
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_BUFF, [RoleId]),
    sql:select(Sql).

%% @doc update
update(Buff) ->
    Sql = parser:format(?UPDATE_BUFF, [
        Buff#buff.start_time,
        Buff#buff.end_time,
        Buff#buff.overlap,
        Buff#buff.role_id,
        Buff#buff.buff_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, BuffId) ->
    Sql = parser:format(?DELETE_BUFF, [RoleId, BuffId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Buff) -> [
        Buff#buff.role_id,
        Buff#buff.buff_id,
        Buff#buff.start_time,
        Buff#buff.end_time,
        Buff#buff.overlap
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_BUFF, #buff.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join(RoleId) ->
    Sql = parser:format(?SELECT_JOIN_BUFF, [RoleId]),
    sql:select(Sql).

