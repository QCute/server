-module(daily_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("daily.hrl").
-define(INSERT_DAILY, <<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_DAILY, <<"SELECT `role_id`, `daily_id`, `is_award`, 0 AS `flag` FROM `daily` WHERE `role_id` = ~w AND `daily_id` = ~w">>).
-define(UPDATE_DAILY, {<<"UPDATE `daily` SET ~i~i~i`is_award` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `daily_id` = ~w">>}).
-define(DELETE_DAILY, <<"DELETE  FROM `daily` WHERE `role_id` = ~w AND `daily_id` = ~w">>).
-define(INSERT_UPDATE_DAILY, {<<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `is_award` = VALUES(`is_award`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `daily_id`, `is_award`, 0 AS `flag` FROM `daily` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `daily`.`role_id`, `daily`.`daily_id`, `daily`.`is_award`, IFNULL(`daily`.`flag`, 0) AS `flag` FROM `daily` WHERE `daily`.`role_id` = ~w">>).

%% @doc insert
insert(Daily) ->
    Sql = parser:format(?INSERT_DAILY, Daily),
    db:insert(Sql).

%% @doc select
select(RoleId, DailyId) ->
    Sql = parser:format(?SELECT_DAILY, [RoleId, DailyId]),
    Data = db:select(Sql),
    parser:convert(Data, daily).

%% @doc update
update(Daily) ->
    Sql = <<(parser:format(element(1, ?UPDATE_DAILY), Daily))/binary, (parser:format(element(2, ?UPDATE_DAILY), [Daily#daily.role_id, Daily#daily.daily_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, DailyId) ->
    Sql = parser:format(?DELETE_DAILY, [RoleId, DailyId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_DAILY, #daily.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, daily).

