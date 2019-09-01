-module(skill_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("skill.hrl").

-define(INSERT_SKILL, <<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES ('~w', '~w', '~w')">>).
-define(UPDATE_SKILL, <<"UPDATE `skill` SET `skill_id` = '~w', `level` = '~w' WHERE `role_id` = '~w'">>).
-define(SELECT_SKILL, <<"SELECT * FROM `skill` WHERE `role_id` = '~w'">>).
-define(DELETE_SKILL, <<"DELETE  FROM `skill` WHERE `role_id` = '~w'">>).
-define(UPDATE_INTO_SKILL, {<<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES ">>, <<"('~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `skill_id` = VALUES(`skill_id`), `level` = VALUES(`level`)">>}).

%% @doc update_into
update_into(DataList) ->
    F = fun(Skill) -> [
        Skill#skill.role_id,
        Skill#skill.skill_id,
        Skill#skill.level
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_SKILL, #skill.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Skill) ->
    Sql = parser:format(?INSERT_SKILL, [
        Skill#skill.role_id,
        Skill#skill.skill_id,
        Skill#skill.level
    ]),
    sql:insert(Sql).

%% @doc update
update(Skill) ->
    Sql = parser:format(?UPDATE_SKILL, [
        Skill#skill.skill_id,
        Skill#skill.level,
        Skill#skill.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_SKILL, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_SKILL, [
        RoleId
    ]),
    sql:delete(Sql).

