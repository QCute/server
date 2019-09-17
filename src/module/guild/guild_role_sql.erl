-module(guild_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD_ROLE, <<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(SELECT_GUILD_ROLE, <<"SELECT * FROM `guild_role`">>).
-define(UPDATE_GUILD_ROLE, <<"UPDATE `guild_role` SET `job` = '~w', `join_time` = '~w', `leave_time` = '~w' WHERE `guild_id` = '~w' AND `role_id` = '~w'">>).
-define(DELETE_GUILD_ROLE, <<"DELETE  FROM `guild_role` WHERE `guild_id` = '~w' AND `role_id` = '~w'">>).
-define(INSERT_UPDATE_GUILD_ROLE, {<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `job` = VALUES(`job`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)">>}).
-define(SELECT_JOIN_GUILD_ROLE, <<"SELECT IFNULL(`guild_role`.`guild_id`, 0), IFNULL(`guild_role`.`role_id`, 0), IFNULL(`guild_role`.`job`, 0), IFNULL(`guild_role`.`join_time`, 0), IFNULL(`guild_role`.`leave_time`, 0), IFNULL(`guild`.`guild_name`, ''), IFNULL(`role`.`role_name`, ''), IFNULL(`role`.`sex`, 0), IFNULL(`role`.`classes`, 0), IFNULL(`vip`.`vip_level`, 0), IFNULL(`guild_role`.`flag`, 0) FROM `guild_role`LEFT JOIN `guild` ON `guild_role`.`guild_id` = `guild`.`guild_id`LEFT JOIN `role` ON `guild_role`.`role_id` = `role`.`role_id`LEFT JOIN `vip` ON `guild_role`.`role_id` = `vip`.`role_id`">>).
-define(DELETE_ROLE_ID, <<"DELETE  FROM `guild_role` WHERE `role_id` = '~w'">>).
-define(DELETE_GUILD_ID, <<"DELETE  FROM `guild_role` WHERE `guild_id` = '~w'">>).

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
    sql:select(Sql).

%% @doc update
update(GuildRole) ->
    Sql = parser:format(?UPDATE_GUILD_ROLE, [
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time,
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(GuildId, RoleId) ->
    Sql = parser:format(?DELETE_GUILD_ROLE, [GuildId, RoleId]),
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
    sql:select(Sql).

%% @doc delete
delete_role_id(RoleId) ->
    Sql = parser:format(?DELETE_ROLE_ID, [RoleId]),
    sql:delete(Sql).

%% @doc delete
delete_guild_id(GuildId) ->
    Sql = parser:format(?DELETE_GUILD_ID, [GuildId]),
    sql:delete(Sql).

