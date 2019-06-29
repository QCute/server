-module(quest_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").

-define(UPDATE_INTO_QUEST, {"INSERT INTO `quest` (`role_id`, `quest_id`, `group_id`, `progress`, `award`) VALUES ", "('~w', '~w', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `group_id` = VALUES(`group_id`), `progress` = VALUES(`progress`), `award` = VALUES(`award`)"}).
-define(INSERT_QUEST, "INSERT INTO `quest` (`role_id`, `quest_id`, `group_id`, `progress`, `award`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_QUEST, "UPDATE `quest` SET `group_id` = '~w', `progress` = '~w', `award` = '~w' WHERE `role_id` = '~w' AND `quest_id` = '~w'").
-define(SELECT_QUEST, "SELECT * FROM `quest` WHERE `role_id` = '~w'").
-define(DELETE_QUEST, "DELETE  FROM `quest` WHERE `role_id` = '~w' AND `quest_id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Quest) -> [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.group_id,
        Quest#quest.progress,
        Quest#quest.award
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_QUEST, #quest.extra),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Quest) ->
    Sql = io_lib:format(?INSERT_QUEST, [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.group_id,
        Quest#quest.progress,
        Quest#quest.award
    ]),
    sql:insert(Sql).

%% @doc update
update(Quest) ->
    Sql = io_lib:format(?UPDATE_QUEST, [
        Quest#quest.group_id,
        Quest#quest.progress,
        Quest#quest.award,
        Quest#quest.role_id,
        Quest#quest.quest_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_QUEST, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId, QuestId) ->
    Sql = io_lib:format(?DELETE_QUEST, [
        RoleId,
        QuestId
    ]),
    sql:delete(Sql).

