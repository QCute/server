-module(buff_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").

-define(INSERT_BUFF, <<"INSERT INTO `buff` (`role_id`, `buff_id`, `start_time`, `end_time`, `overlap`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(UPDATE_BUFF, <<"UPDATE `buff` SET `start_time` = '~w', `end_time` = '~w', `overlap` = '~w' WHERE `role_id` = '~w' AND `buff_id` = '~w'">>).
-define(SELECT_BUFF, <<"SELECT * FROM `buff` WHERE `role_id` = '~w'">>).
-define(DELETE_BUFF, <<"DELETE  FROM `buff` WHERE `role_id` = '~w' AND `buff_id` = '~w'">>).
-define(UPDATE_INTO_BUFF, {<<"INSERT INTO `buff` (`role_id`, `buff_id`, `start_time`, `end_time`, `overlap`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `start_time` = VALUES(`start_time`), `end_time` = VALUES(`end_time`), `overlap` = VALUES(`overlap`)">>}).

%% @doc update_into
update_into(DataList) ->
    F = fun(Buff) -> [
        Buff#buff.role_id,
        Buff#buff.buff_id,
        Buff#buff.start_time,
        Buff#buff.end_time,
        Buff#buff.overlap
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_BUFF, #buff.flag),
    sql:insert(Sql),
    NewData.


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

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_BUFF, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId, BuffId) ->
    Sql = parser:format(?DELETE_BUFF, [
        RoleId,
        BuffId
    ]),
    sql:delete(Sql).

