-module(dungeon_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("dungeon.hrl").
-define(INSERT_DUNGEON, <<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_DUNGEON, <<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, 0 AS `flag` FROM `dungeon` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_DUNGEON, {<<"UPDATE `dungeon` SET ~i~i`dungeon_id` = ~w, ~i`today_number` = ~w, `total_number` = ~w, `is_pass` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_DUNGEON, <<"DELETE  FROM `dungeon` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_DUNGEON, {<<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `dungeon_id` = VALUES(`dungeon_id`), `today_number` = VALUES(`today_number`), `total_number` = VALUES(`total_number`), `is_pass` = VALUES(`is_pass`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, 0 AS `flag` FROM `dungeon` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `dungeon`.`role_id`, `dungeon`.`dungeon_id`, `dungeon`.`type`, `dungeon`.`today_number`, `dungeon`.`total_number`, `dungeon`.`is_pass`, IFNULL(`dungeon`.`flag`, 0) AS `flag` FROM `dungeon` WHERE `dungeon`.`role_id` = ~w">>).

%% @doc insert
insert(Dungeon) ->
    Sql = parser:format(?INSERT_DUNGEON, Dungeon),
    db:insert(Sql).

%% @doc select
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_DUNGEON, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, dungeon).

%% @doc update
update(Dungeon) ->
    Sql = <<(parser:format(element(1, ?UPDATE_DUNGEON), Dungeon))/binary, (parser:format(element(2, ?UPDATE_DUNGEON), [Dungeon#dungeon.role_id, Dungeon#dungeon.type]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_DUNGEON, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_DUNGEON, #dungeon.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, dungeon).

