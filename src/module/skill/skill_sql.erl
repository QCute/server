-module(skill_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("skill.hrl").

-define(INSERT_SKILL, <<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_SKILL, <<"SELECT `role_id`, `skill_id`, `level`, 0 AS `flag` FROM `skill` WHERE `role_id` = ~w AND `skill_id` = ~w">>).
-define(UPDATE_SKILL, {<<"UPDATE `skill` SET ~i~i~i`level` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `skill_id` = ~w">>}).
-define(DELETE_SKILL, <<"DELETE FROM `skill` WHERE `role_id` = ~w AND `skill_id` = ~w">>).
-define(INSERT_UPDATE_SKILL, {<<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `level` = VALUES(`level`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `skill_id`, `level`, 0 AS `flag` FROM `skill` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `skill`.`role_id`, `skill`.`skill_id`, `skill`.`level`, IFNULL(`skill`.`flag`, 0) AS `flag` FROM `skill` WHERE `skill`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Skill :: #skill{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Skill) ->
    Sql = parser:format(?INSERT_SKILL, Skill),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), SkillId :: integer()) -> SkillList :: [#skill{}].
select(RoleId, SkillId) ->
    Sql = parser:format(?SELECT_SKILL, [RoleId, SkillId]),
    Data = db:select(Sql),
    parser:convert(Data, skill).

%% @doc update
-spec update(Skill :: #skill{}) -> AffectedRows :: non_neg_integer().
update(Skill) ->
    Sql = <<(parser:format(element(1, ?UPDATE_SKILL), Skill))/binary, (parser:format(element(2, ?UPDATE_SKILL), [Skill#skill.role_id, Skill#skill.skill_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), SkillId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, SkillId) ->
    Sql = parser:format(?DELETE_SKILL, [RoleId, SkillId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(SkillList :: [#skill{}] | ets:tab()) -> NewSkillList :: [#skill{}].
insert_update(SkillList) ->
    {Sql, NewSkillList} = parser:collect_into(SkillList, ?INSERT_UPDATE_SKILL, #skill.flag),
    db:insert(Sql),
    NewSkillList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> SkillList :: [#skill{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, skill).

