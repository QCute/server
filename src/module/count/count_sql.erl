-module(count_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("count.hrl").
-define(INSERT_COUNT, <<"INSERT INTO `count` (`role_id`, `type`, `today_number`, `total_number`) VALUES ('~w', '~w', '~w', '~w')">>).
-define(SELECT_COUNT, <<"SELECT * FROM `count` WHERE `role_id` = '~w'">>).
-define(UPDATE_COUNT, <<"UPDATE `count` SET `today_number` = '~w', `total_number` = '~w' WHERE `role_id` = '~w' AND `type` = '~w'">>).
-define(DELETE_COUNT, <<"DELETE  FROM `count` WHERE `role_id` = '~w' AND `type` = '~w'">>).

%% @doc insert
insert(Count) ->
    Sql = parser:format(?INSERT_COUNT, [
        Count#count.role_id,
        Count#count.type,
        Count#count.today_number,
        Count#count.total_number
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_COUNT, [RoleId]),
    sql:select(Sql).

%% @doc update
update(Count) ->
    Sql = parser:format(?UPDATE_COUNT, [
        Count#count.today_number,
        Count#count.total_number,
        Count#count.role_id,
        Count#count.type
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_COUNT, [RoleId, Type]),
    sql:delete(Sql).

