-module(achievement_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("achievement.hrl").

-define(INSERT_ACHIEVEMENT, <<"INSERT INTO `achievement` (`role_id`, `achievement_id`, `type`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_ACHIEVEMENT, <<"SELECT `role_id`, `achievement_id`, `type`, 0 AS `flag` FROM `achievement` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_ACHIEVEMENT, {<<"UPDATE `achievement` SET ~i~i`achievement_id` = ~w~i~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_ACHIEVEMENT, <<"DELETE FROM `achievement` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_ACHIEVEMENT, {<<"INSERT INTO `achievement` (`role_id`, `achievement_id`, `type`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `achievement_id` = VALUES(`achievement_id`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `achievement_id`, `type`, 0 AS `flag` FROM `achievement` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `achievement`.`role_id`, `achievement`.`achievement_id`, `achievement`.`type`, IFNULL(`achievement`.`flag`, 0) AS `flag` FROM `achievement` WHERE `achievement`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Achievement :: #achievement{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Achievement) ->
    Sql = parser:format(?INSERT_ACHIEVEMENT, Achievement),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), Type :: integer()) -> AchievementList :: [#achievement{}].
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_ACHIEVEMENT, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, achievement).

%% @doc update
-spec update(Achievement :: #achievement{}) -> AffectedRows :: non_neg_integer().
update(Achievement) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ACHIEVEMENT), Achievement))/binary, (parser:format(element(2, ?UPDATE_ACHIEVEMENT), [Achievement#achievement.role_id, Achievement#achievement.type]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), Type :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_ACHIEVEMENT, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(AchievementList :: [#achievement{}] | ets:tab()) -> NewAchievementList :: [#achievement{}].
insert_update(AchievementList) ->
    {Sql, NewAchievementList} = parser:collect_into(AchievementList, ?INSERT_UPDATE_ACHIEVEMENT, #achievement.flag),
    db:insert(Sql),
    NewAchievementList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> AchievementList :: [#achievement{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, achievement).

