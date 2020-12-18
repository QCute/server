-module(guild_apply_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD_APPLY, <<"INSERT INTO `guild_apply` (`guild_id`, `role_id`, `apply_time`) VALUES (~w, ~w, ~w)">>).
-define(SELECT_GUILD_APPLY, <<"SELECT `guild_id`, `role_id`, `apply_time`, '' AS `guild_name`, '' AS `role_name`, 0 AS `sex`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `flag` FROM `guild_apply`">>).
-define(UPDATE_GUILD_APPLY, <<"UPDATE `guild_apply` SET `apply_time` = ~w WHERE `guild_id` = ~w AND `role_id` = ~w">>).
-define(DELETE_GUILD_APPLY, <<"DELETE  FROM `guild_apply` WHERE `guild_id` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_GUILD_APPLY, {<<"INSERT INTO `guild_apply` (`guild_id`, `role_id`, `apply_time`) VALUES ">>, <<"(~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `apply_time` = VALUES(`apply_time`)">>}).
-define(SELECT_JOIN_GUILD_APPLY, <<"SELECT `guild_apply`.`guild_id`, `guild_apply`.`role_id`, `guild_apply`.`apply_time`, IFNULL(`guild`.`guild_name`, '') AS `guild_name`, IFNULL(`role`.`role_name`, '') AS `role_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`guild_apply`.`flag`, 0) AS `flag` FROM `guild_apply` LEFT JOIN `guild` ON `guild_apply`.`guild_id` = `guild`.`guild_id` LEFT JOIN `role` ON `guild_apply`.`role_id` = `role`.`role_id` LEFT JOIN `vip` ON `guild_apply`.`role_id` = `vip`.`role_id`">>).
-define(DELETE_BY_GUILD_ID, <<"DELETE FROM `guild_apply` WHERE `guild_id` = ~w">>).
-define(DELETE_BY_ROLE_ID, <<"DELETE FROM `guild_apply` WHERE `role_id` = ~w">>).

%% @doc insert
insert(GuildApply) ->
    Sql = parser:format(?INSERT_GUILD_APPLY, [
        GuildApply#guild_apply.guild_id,
        GuildApply#guild_apply.role_id,
        GuildApply#guild_apply.apply_time
    ]),
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD_APPLY, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_apply).

%% @doc update
update(GuildApply) ->
    Sql = parser:format(?UPDATE_GUILD_APPLY, [
        GuildApply#guild_apply.apply_time,
        GuildApply#guild_apply.guild_id,
        GuildApply#guild_apply.role_id
    ]),
    db:update(Sql).

%% @doc delete
delete(GuildId, RoleId) ->
    Sql = parser:format(?DELETE_GUILD_APPLY, [GuildId, RoleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(GuildApply) -> [
        GuildApply#guild_apply.guild_id,
        GuildApply#guild_apply.role_id,
        GuildApply#guild_apply.apply_time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_GUILD_APPLY, #guild_apply.flag),
    db:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_APPLY, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_apply).

%% @doc delete
delete_by_guild_id(GuildId) ->
    Sql = parser:format(?DELETE_BY_GUILD_ID, [GuildId]),
    db:delete(Sql).

%% @doc delete
delete_by_role_id(RoleId) ->
    Sql = parser:format(?DELETE_BY_ROLE_ID, [RoleId]),
    db:delete(Sql).

