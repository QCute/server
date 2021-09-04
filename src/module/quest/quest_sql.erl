-module(quest_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").
-define(INSERT_QUEST, <<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `number`, `is_award`) VALUES (~i~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_QUEST, <<"SELECT `role_id`, `quest_id`, `type`, `number`, `is_award`, 0 AS `flag` FROM `quest` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_QUEST, {<<"UPDATE `quest` SET ~i~i`quest_id` = ~w, ~i`number` = ~w, `is_award` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_QUEST, <<"DELETE  FROM `quest` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_QUEST, {<<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `number`, `is_award`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `quest_id` = VALUES(`quest_id`), `number` = VALUES(`number`), `is_award` = VALUES(`is_award`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `quest_id`, `type`, `number`, `is_award`, 0 AS `flag` FROM `quest` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `quest`.`role_id`, `quest`.`quest_id`, `quest`.`type`, `quest`.`number`, `quest`.`is_award`, IFNULL(`quest`.`flag`, 0) AS `flag` FROM `quest` WHERE `quest`.`role_id` = ~w">>).

%% @doc insert
insert(Quest) ->
    Sql = parser:format(?INSERT_QUEST, Quest),
    db:insert(Sql).

%% @doc select
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_QUEST, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, quest).

%% @doc update
update(Quest) ->
    Sql = <<(parser:format(element(1, ?UPDATE_QUEST), Quest))/binary, (parser:format(element(2, ?UPDATE_QUEST), [Quest#quest.role_id, Quest#quest.type]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_QUEST, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_QUEST, #quest.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, quest).

