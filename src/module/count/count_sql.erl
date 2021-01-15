-module(count_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("count.hrl").
-define(INSERT_COUNT, <<"INSERT INTO `count` (`role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_COUNT, <<"SELECT `role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`, 0 AS `flag` FROM `count` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_COUNT, {<<"UPDATE `count` SET ~i~i~i`today_number` = ~w, `week_number` = ~w, `total_number` = ~w, `time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_COUNT, <<"DELETE  FROM `count` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_COUNT, {<<"INSERT INTO `count` (`role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `today_number` = VALUES(`today_number`), `week_number` = VALUES(`week_number`), `total_number` = VALUES(`total_number`), `time` = VALUES(`time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`, 0 AS `flag` FROM `count` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `count`.`role_id`, `count`.`type`, `count`.`today_number`, `count`.`week_number`, `count`.`total_number`, `count`.`time`, IFNULL(`count`.`flag`, 0) AS `flag` FROM `count` WHERE `count`.`role_id` = ~w">>).

%% @doc insert
insert(Count) ->
    Sql = parser:format(?INSERT_COUNT, Count),
    db:insert(Sql).

%% @doc select
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_COUNT, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, count).

%% @doc update
update(Count) ->
    Sql = <<(parser:format(element(1, ?UPDATE_COUNT), Count))/binary, (parser:format(element(2, ?UPDATE_COUNT), [Count#count.role_id, Count#count.type]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_COUNT, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_COUNT, #count.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, count).

