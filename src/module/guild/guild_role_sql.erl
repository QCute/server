-module(guild_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD_ROLE, <<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(DELETE_GUILD_ROLE, <<"DELETE  FROM `guild_role` WHERE `guild_id` = '~w' AND `role_id` = '~w'">>).
-define(SELECT_GUILD_ROLE, <<"SELECT * FROM `guild_role` ">>).
-define(UPDATE_GUILD_ROLE, <<"UPDATE `guild_role` SET `job` = '~w', `join_time` = '~w', `leave_time` = '~w' WHERE `guild_id` = '~w' AND `role_id` = '~w'">>).
-define(INSERT_UPDATE_GUILD_ROLE, {<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `join_time`, `leave_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `job` = '~w', `join_time` = '~w', `leave_time` = '~w'">>}).
-define(SELECT_JOIN_GUILD_ROLE, <<"SELECT `guild`.`guild_id`, `role`.`role_id`, `guild_role`.`job`, `guild_role`.`join_time`, `guild_role`.`leave_time`, `guild`.`guild_name`, `role`.`role_name`, `guild_role`.`role_pid`, `guild_role`.`role_sender_pid`, `guild_role`.`flag` FROM `guild_role` ">>).

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

%% @doc delete
delete(GuildId, RoleId) ->
    Sql = parser:format(?DELETE_GUILD_ROLE, [GuildId, RoleId]),
    sql:delete(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD_ROLE, []),
    sql:select(Sql).

%% @doc update
update(GuildRole) ->
    Sql = parser:format(?UPDATE_GUILD_ROLE, [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id
    ]),
    sql:update(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(GuildRole) -> [
        GuildRole#guild_role.guild_id,
        GuildRole#guild_role.role_id,
        GuildRole#guild_role.job,
        GuildRole#guild_role.join_time,
        GuildRole#guild_role.leave_time
    ] end,
    {Sql, NewData} = parser:collect(Data, F, ?INSERT_UPDATE_GUILD_ROLE, flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_ROLE, []),
    sql:select(Sql).

