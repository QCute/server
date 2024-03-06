-module(skill_sql).
-export([save/1]).
-export([select/1]).
-include("skill.hrl").

%% @doc insert into skill
-spec save(SkillList :: [#skill{}] | ets:tab()) -> NewSkillList :: [#skill{}].
save(SkillList) ->
    db:save_into(<<"INSERT INTO `skill` (`role_id`, `skill_id`, `level`) VALUES">>, <<"(:2:, :3:, :4:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `skill_id` = VALUES(`skill_id`), `level` = VALUES(`level`)">>, SkillList, #skill.flag).

%% @doc select from skill
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#skill{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `skill_id`, `level`, `flag` FROM `skill` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, skill).
