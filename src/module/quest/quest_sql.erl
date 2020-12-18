-module(quest_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").
-define(INSERT_QUEST, <<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `target`, `number`, `is_award`) VALUES (~w, ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_QUEST, <<"SELECT `role_id`, `quest_id`, `type`, `target`, `number`, `is_award`, 0 AS `flag` FROM `quest` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_QUEST, <<"UPDATE `quest` SET `quest_id` = ~w, `target` = ~w, `number` = ~w, `is_award` = ~w WHERE `role_id` = ~w AND `type` = ~w">>).
-define(DELETE_QUEST, <<"DELETE  FROM `quest` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_QUEST, {<<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `target`, `number`, `is_award`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `quest_id` = VALUES(`quest_id`), `target` = VALUES(`target`), `number` = VALUES(`number`), `is_award` = VALUES(`is_award`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `quest_id`, `type`, `target`, `number`, `is_award`, 0 AS `flag` FROM `quest` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `quest`.`role_id`, `quest`.`quest_id`, `quest`.`type`, `quest`.`target`, `quest`.`number`, `quest`.`is_award`, IFNULL(`quest`.`flag`, 0) AS `flag` FROM `quest` WHERE `quest`.`role_id` = ~w">>).

%% @doc insert
insert(Quest) ->
    Sql = parser:format(?INSERT_QUEST, [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.type,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.is_award
    ]),
    db:insert(Sql).

%% @doc select
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_QUEST, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, quest).

%% @doc update
update(Quest) ->
    Sql = parser:format(?UPDATE_QUEST, [
        Quest#quest.quest_id,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.is_award,
        Quest#quest.role_id,
        Quest#quest.type
    ]),
    db:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_QUEST, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Quest) -> [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.type,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.is_award
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_QUEST, #quest.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, quest).

