-module(dungeon_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("dungeon.hrl").

-define(INSERT_DUNGEON, <<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_DUNGEON, <<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, 0 AS `flag` FROM `dungeon` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_DUNGEON, {<<"UPDATE `dungeon` SET ~i~i`dungeon_id` = ~w, ~i`today_number` = ~w, `total_number` = ~w, `is_pass` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_DUNGEON, <<"DELETE FROM `dungeon` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_DUNGEON, {<<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `dungeon_id` = VALUES(`dungeon_id`), `today_number` = VALUES(`today_number`), `total_number` = VALUES(`total_number`), `is_pass` = VALUES(`is_pass`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, 0 AS `flag` FROM `dungeon` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `dungeon`.`role_id`, `dungeon`.`dungeon_id`, `dungeon`.`type`, `dungeon`.`today_number`, `dungeon`.`total_number`, `dungeon`.`is_pass`, IFNULL(`dungeon`.`flag`, 0) AS `flag` FROM `dungeon` WHERE `dungeon`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Dungeon :: #dungeon{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Dungeon) ->
    Sql = parser:format(?INSERT_DUNGEON, Dungeon),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), Type :: integer()) -> DungeonList :: [#dungeon{}].
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_DUNGEON, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, dungeon).

%% @doc update
-spec update(Dungeon :: #dungeon{}) -> AffectedRows :: non_neg_integer().
update(Dungeon) ->
    Sql = <<(parser:format(element(1, ?UPDATE_DUNGEON), Dungeon))/binary, (parser:format(element(2, ?UPDATE_DUNGEON), [Dungeon#dungeon.role_id, Dungeon#dungeon.type]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), Type :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_DUNGEON, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(DungeonList :: [#dungeon{}] | ets:tab()) -> NewDungeonList :: [#dungeon{}].
insert_update(DungeonList) ->
    {Sql, NewDungeonList} = parser:collect_into(DungeonList, ?INSERT_UPDATE_DUNGEON, #dungeon.flag),
    db:insert(Sql),
    NewDungeonList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> DungeonList :: [#dungeon{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, dungeon).

