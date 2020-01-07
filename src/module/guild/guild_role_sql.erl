-module(guild_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD_ROLE, <<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES (~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_GUILD_ROLE, <<"SELECT `guild_id`, `role_id`, `job`, `join_time`, `leave_time`, `guild_name`, `role_name`, `sex`, `classes`, `level`, `vip_level`, 0 AS `flag` FROM `guild_role`">>).
-define(UPDATE_GUILD_ROLE, <<"UPDATE `guild_role` SET `guild_id` = ~w, `job` = ~w, `join_time` = ~w, `leave_time` = ~w WHERE `role_id` = ~w">>).
-define(DELETE_GUILD_ROLE, <<"DELETE  FROM `guild_role` WHERE `role_id` = ~w">>).
-define(INSERT_UPDATE_GUILD_ROLE, {<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `job` = VALUES(`job`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)">>}).
-define(SELECT_JOIN_GUILD_ROLE, <<"SELECT `guild`.`guild_id`, `role`.`role_id`, `guild_role`.`job`, `guild_role`.`join_time`, `guild_role`.`leave_time`, IFNULL(`guild`.`guild_name`, '') AS `guild_name`, IFNULL(`role`.`role_name`, '') AS `role_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, 0 AS `flag` FROM `guild_role`LEFT JOIN `guild` ON `guild_role`.`guild_id` = `guild`.`guild_id`LEFT JOIN `role` ON `guild_role`.`role_id` = `role`.`role_id`LEFT JOIN `vip` ON `guild_role`.`role_id` = `vip`.`role_id`">>).
-define(DELETE_ROLE_ID, <<"DELETE  FROM `guild_role` WHERE `role_id` = ~w">>).
-define(DELETE_GUILD_ID, <<"DELETE  FROM `guild_role` WHERE `guild_id` = ~w">>).

%% @doc insert
insert(GuildRole) ->
    Sql = parser:format(?INSERT_GUILD_ROLE, [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD_ROLE, []),
    Data = sql:select(Sql),
    parser:convert(Data, guild_role).

%% @doc update
update(GuildRole) ->
    Sql = parser:format(?UPDATE_GUILD_ROLE, [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time,
        GuildRole#guild_role.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_GUILD_ROLE, [RoleId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(GuildRole) -> [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_GUILD_ROLE, #guild_role.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_ROLE, []),
    Data = sql:select(Sql),
    parser:convert(Data, guild_role).

%% @doc delete
delete_role_id(RoleId) ->
    Sql = parser:format(?DELETE_ROLE_ID, [RoleId]),
    sql:delete(Sql).

%% @doc delete
delete_guild_id(GuildId) ->
    Sql = parser:format(?DELETE_GUILD_ID, [GuildId]),
    sql:delete(Sql).

