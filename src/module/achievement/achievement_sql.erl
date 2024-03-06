-module(achievement_sql).
-export([save/1]).
-export([select/1]).
-include("achievement.hrl").

%% @doc insert into achievement
-spec save(AchievementList :: [#achievement{}] | ets:tab()) -> NewAchievementList :: [#achievement{}].
save(AchievementList) ->
    db:save_into(<<"INSERT INTO `achievement` (`role_id`, `achievement_id`, `type`) VALUES">>, <<"(?, ?, ?)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `achievement_id` = VALUES(`achievement_id`), `type` = VALUES(`type`)">>, AchievementList, fun(#achievement{role_id = RoleId, achievement_id = AchievementId, type = Type}) -> [RoleId, AchievementId, Type] end, #achievement.flag).

%% @doc select from achievement
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#achievement{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `achievement_id`, `type`, `flag` FROM `achievement` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, achievement).
