-module(guild_apply_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").

-define(INSERT_GUILD_APPLY, <<"INSERT INTO `guild_apply` (`role_id`, `guild_id`, `time`) VALUES ('~w', '~w', '~w')">>).
-define(UPDATE_GUILD_APPLY, <<"UPDATE `guild_apply` SET `time` = '~w' WHERE `role_id` = '~w' AND `guild_id` = '~w'">>).
-define(SELECT_GUILD_APPLY, <<"SELECT * FROM `guild_apply` ">>).
-define(DELETE_GUILD_APPLY, <<"DELETE  FROM `guild_apply` WHERE `role_id` = '~w' AND `guild_id` = '~w'">>).
-define(UPDATE_INTO_GUILD_APPLY, {<<"INSERT INTO `guild_apply` (`role_id`, `guild_id`, `time`) VALUES ">>, <<"('~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `time` = VALUES(`time`)">>}).
-define(SELECT_JOIN_GUILD_APPLY, <<"SELECT IFNULL(`role`.`role_id`, 0), `guild_apply`.`guild_id`, `guild_apply`.`time`, IFNULL(`role`.`role_name`, ''), `guild_apply`.`role_pid`, `guild_apply`.`sender_pid`, `guild_apply`.`flag` FROM `guild_apply`  LEFT JOIN `role` ON `guild_apply`.`role_id` = `role`.`role_id` ">>).
-define(DELETE_GUILD_APPLY_ROLE, <<"DELETE  FROM `guild_apply` WHERE `role_id` = '~w'">>).
-define(DELETE_GUILD_APPLY_GUILD, <<"DELETE  FROM `guild_apply` WHERE `guild_id` = '~w'">>).

%% @doc update_into
update_into(DataList) ->
    F = fun(GuildApply) -> [
        GuildApply#guild_apply.role_id,
        GuildApply#guild_apply.guild_id,
        GuildApply#guild_apply.time
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_GUILD_APPLY, #guild_apply.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(GuildApply) ->
    Sql = parser:format(?INSERT_GUILD_APPLY, [
        GuildApply#guild_apply.role_id,
        GuildApply#guild_apply.guild_id,
        GuildApply#guild_apply.time
    ]),
    sql:insert(Sql).

%% @doc update
update(GuildApply) ->
    Sql = parser:format(?UPDATE_GUILD_APPLY, [
        GuildApply#guild_apply.time,
        GuildApply#guild_apply.role_id,
        GuildApply#guild_apply.guild_id
    ]),
    sql:update(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD_APPLY, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId, GuildId) ->
    Sql = parser:format(?DELETE_GUILD_APPLY, [
        RoleId,
        GuildId
    ]),
    sql:delete(Sql).

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_APPLY, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete_role(RoleId) ->
    Sql = parser:format(?DELETE_GUILD_APPLY_ROLE, [
        RoleId
    ]),
    sql:delete(Sql).

%% @doc delete
delete_guild(GuildId) ->
    Sql = parser:format(?DELETE_GUILD_APPLY_GUILD, [
        GuildId
    ]),
    sql:delete(Sql).

