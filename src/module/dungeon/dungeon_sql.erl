-module(dungeon_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("dungeon.hrl").
-define(INSERT_DUNGEON, <<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES (~w, ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_DUNGEON, <<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, `flag` FROM `dungeon` WHERE `role_id` = ~w">>).
-define(UPDATE_DUNGEON, <<"UPDATE `dungeon` SET `dungeon_id` = ~w, `today_number` = ~w, `total_number` = ~w, `is_pass` = ~w WHERE `role_id` = ~w AND `type` = ~w">>).
-define(DELETE_DUNGEON, <<"DELETE  FROM `dungeon` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_DUNGEON, {<<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `dungeon_id` = VALUES(`dungeon_id`), `today_number` = VALUES(`today_number`), `total_number` = VALUES(`total_number`), `is_pass` = VALUES(`is_pass`)">>}).
-define(TRUNCATE, <<"TRUNCATE TABLE `dungeon`">>).

%% @doc insert
insert(Dungeon) ->
    Sql = parser:format(?INSERT_DUNGEON, [
        Dungeon#dungeon.role_id,
        Dungeon#dungeon.dungeon_id,
        Dungeon#dungeon.type,
        Dungeon#dungeon.today_number,
        Dungeon#dungeon.total_number,
        Dungeon#dungeon.is_pass
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_DUNGEON, [RoleId]),
    Data = sql:select(Sql),
    parser:convert(Data, dungeon).

%% @doc update
update(Dungeon) ->
    Sql = parser:format(?UPDATE_DUNGEON, [
        Dungeon#dungeon.dungeon_id,
        Dungeon#dungeon.today_number,
        Dungeon#dungeon.total_number,
        Dungeon#dungeon.is_pass,
        Dungeon#dungeon.role_id,
        Dungeon#dungeon.type
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_DUNGEON, [RoleId, Type]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Dungeon) -> [
        Dungeon#dungeon.role_id,
        Dungeon#dungeon.dungeon_id,
        Dungeon#dungeon.type,
        Dungeon#dungeon.today_number,
        Dungeon#dungeon.total_number,
        Dungeon#dungeon.is_pass
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_DUNGEON, #dungeon.flag),
    sql:insert(Sql),
    NewData.

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

