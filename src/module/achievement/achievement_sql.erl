-module(achievement_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("achievement.hrl").
-define(INSERT_ACHIEVEMENT, <<"INSERT INTO `achievement` (`role_id`, `achievement_id`, `type`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_ACHIEVEMENT, <<"SELECT `role_id`, `achievement_id`, `type`, 0 AS `flag` FROM `achievement` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_ACHIEVEMENT, {<<"UPDATE `achievement` SET ~i~i`achievement_id` = ~w~i~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_ACHIEVEMENT, <<"DELETE  FROM `achievement` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_ACHIEVEMENT, {<<"INSERT INTO `achievement` (`role_id`, `achievement_id`, `type`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `achievement_id` = VALUES(`achievement_id`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `achievement_id`, `type`, 0 AS `flag` FROM `achievement` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `achievement`.`role_id`, `achievement`.`achievement_id`, `achievement`.`type`, IFNULL(`achievement`.`flag`, 0) AS `flag` FROM `achievement` WHERE `achievement`.`role_id` = ~w">>).

%% @doc insert
insert(Achievement) ->
    Sql = parser:format(?INSERT_ACHIEVEMENT, Achievement),
    db:insert(Sql).

%% @doc select
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_ACHIEVEMENT, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, achievement).

%% @doc update
update(Achievement) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ACHIEVEMENT), Achievement))/binary, (parser:format(element(2, ?UPDATE_ACHIEVEMENT), [Achievement#achievement.role_id, Achievement#achievement.type]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_ACHIEVEMENT, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_ACHIEVEMENT, #achievement.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, achievement).

