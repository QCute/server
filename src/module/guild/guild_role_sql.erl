-module(guild_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").

-define(SELECT_JOIN_GUILD_role, "SELECT IFNULL(`guild`.`guild_id`, 0), IFNULL(`role`.`role_id`, 0), `guild_role`.`job`, `guild_role`.`join_time`, `guild_role`.`leave_time`, IFNULL(`guild`.`guild_name`, ''), IFNULL(`role`.`role_name`, ''), `guild_role`.`role_pid`, `guild_role`.`role_sender_pid`, `guild_role`.`extra` FROM `guild_role`  LEFT JOIN `guild` ON `guild_role`.`guild_id` = `guild`.`guild_id` LEFT JOIN `role` ON `guild_role`.`role_id` = `role`.`role_id` ").
-define(UPDATE_INTO_GUILD_role, {"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ", "('~w', '~w', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `job` = VALUES(`job`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)"}).
-define(INSERT_GUILD_role, "INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_GUILD_role, "UPDATE `guild_role` SET `guild_id` = '~w', `job` = '~w', `join_time` = '~w', `leave_time` = '~w' WHERE `role_id` = '~w'").
-define(SELECT_GUILD_role, "SELECT * FROM `guild_role` ").
-define(DELETE_GUILD_role, "DELETE  FROM `guild_role` WHERE `role_id` = '~w'").
-define(UPDATE_GUILD_role_GUILD_ID, "UPDATE `guild_role` SET `guild_id` = '~w' WHERE `role_id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(GuildRole) -> [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_GUILD_role, #guild_role.extra),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(GuildRole) ->
    Sql = io_lib:format(?INSERT_GUILD_role, [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time
    ]),
    sql:insert(Sql).

%% @doc update
update(GuildRole) ->
    Sql = io_lib:format(?UPDATE_GUILD_role, [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time,
        GuildRole#guild_role.role_id
    ]),
    sql:update(Sql).

%% @doc select
select() ->
    Sql = io_lib:format(?SELECT_GUILD_role, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = io_lib:format(?DELETE_GUILD_role, [
        RoleId
    ]),
    sql:delete(Sql).

%% @doc select join
select_join() ->
    Sql = io_lib:format(?SELECT_JOIN_GUILD_role, [
        
    ]),
    sql:select(Sql).

%% @doc update
update_guild_id(GuildId, RoleId) ->
    Sql = io_lib:format(?UPDATE_GUILD_role_GUILD_ID, [
        GuildId,
        RoleId
    ]),
    sql:update(Sql).

