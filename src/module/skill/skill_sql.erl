-module(skill_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("skill.hrl").
-define(INSERT_SKILL, <<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES (~w, ~w, ~w)">>).
-define(SELECT_SKILL, <<"SELECT `role_id`, `skill_id`, `level`, 0 AS `flag` FROM `skill` WHERE `role_id` = ~w AND `skill_id` = ~w">>).
-define(UPDATE_SKILL, <<"UPDATE `skill` SET `level` = ~w WHERE `role_id` = ~w AND `skill_id` = ~w">>).
-define(DELETE_SKILL, <<"DELETE  FROM `skill` WHERE `role_id` = ~w AND `skill_id` = ~w">>).
-define(INSERT_UPDATE_SKILL, {<<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES ">>, <<"(~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `level` = VALUES(`level`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `skill_id`, `level`, 0 AS `flag` FROM `skill` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `skill`.`role_id`, `skill`.`skill_id`, `skill`.`level`, IFNULL(`skill`.`flag`, 0) AS `flag` FROM `skill` WHERE `skill`.`role_id` = ~w">>).

%% @doc insert
insert(Skill) ->
    Sql = parser:format(?INSERT_SKILL, [
        Skill#skill.role_id,
        Skill#skill.skill_id,
        Skill#skill.level
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId, SkillId) ->
    Sql = parser:format(?SELECT_SKILL, [RoleId, SkillId]),
    Data = sql:select(Sql),
    parser:convert(Data, skill).

%% @doc update
update(Skill) ->
    Sql = parser:format(?UPDATE_SKILL, [
        Skill#skill.level,
        Skill#skill.role_id,
        Skill#skill.skill_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, SkillId) ->
    Sql = parser:format(?DELETE_SKILL, [RoleId, SkillId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Skill) -> [
        Skill#skill.role_id,
        Skill#skill.skill_id,
        Skill#skill.level
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_SKILL, #skill.flag),
    sql:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = sql:select(Sql),
    parser:convert(Data, skill).

