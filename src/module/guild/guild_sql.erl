-module(guild_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD, <<"INSERT INTO `guild` (`exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`) VALUES (~w, ~w, ~w, ~w, '~s', '~s', ~w)">>).
-define(SELECT_GUILD, <<"SELECT `guild_id`, `exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`, `leader_name`, `leader_sex`, `leader_class`, `leader_level`, `leader_vip_level`, 0 AS `flag` FROM `guild`">>).
-define(UPDATE_GUILD, <<"UPDATE `guild` SET `exp` = ~w, `wealth` = ~w, `level` = ~w, `leader_id` = ~w WHERE `guild_id` = ~w">>).
-define(DELETE_GUILD, <<"DELETE  FROM `guild` WHERE `guild_id` = ~w">>).
-define(INSERT_UPDATE_GUILD, {<<"INSERT INTO `guild` (`guild_id`, `exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, '~s', '~s', ~w)">>, <<" ON DUPLICATE KEY UPDATE `exp` = VALUES(`exp`), `wealth` = VALUES(`wealth`), `level` = VALUES(`level`), `create_time` = VALUES(`create_time`), `guild_name` = VALUES(`guild_name`), `notice` = VALUES(`notice`), `leader_id` = VALUES(`leader_id`)">>}).
-define(SELECT_JOIN_GUILD, <<"SELECT `guild`.`guild_id`, `guild`.`exp`, `guild`.`wealth`, `guild`.`level`, `guild`.`create_time`, `guild`.`guild_name`, `guild`.`notice`, `role`.`role_id`, IFNULL(`role`.`role_name`, '') AS `leader_name`, IFNULL(`role`.`sex`, 0) AS `leader_sex`, IFNULL(`role`.`classes`, 0) AS `leader_class`, IFNULL(`role`.`level`, 0) AS `leader_level`, IFNULL(`vip`.`vip_level`, 0) AS `leader_vip_level`, 0 AS `flag` FROM `guild`LEFT JOIN `role` ON `guild`.`leader_id` = `role`.`role_id`LEFT JOIN `vip` ON `guild`.`leader_id` = `vip`.`role_id`">>).
-define(UPDATE_NOTICE, <<"UPDATE `guild` SET `notice` = '~s' WHERE `guild_id` = ~w">>).
-define(UPDATE_NAME, <<"UPDATE `guild` SET `guild_name` = '~s' WHERE `guild_id` = ~w">>).
-define(DELETE_IN_GUILD_ID, {<<"DELETE  FROM `guild` WHERE `guild_id` in (">>, <<"~w">>, <<")">>}).
-define(TRUNCATE, <<"TRUNCATE TABLE `guild`">>).

%% @doc insert
insert(Guild) ->
    Sql = parser:format(?INSERT_GUILD, [
        Guild#guild.exp,
        Guild#guild.wealth,
        Guild#guild.level,
        Guild#guild.create_time,
        Guild#guild.guild_name,
        Guild#guild.notice,
        Guild#guild.leader_id
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD, []),
    Data = sql:select(Sql),
    parser:convert(Data, guild).

%% @doc update
update(Guild) ->
    Sql = parser:format(?UPDATE_GUILD, [
        Guild#guild.exp,
        Guild#guild.wealth,
        Guild#guild.level,
        Guild#guild.leader_id,
        Guild#guild.guild_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(GuildId) ->
    Sql = parser:format(?DELETE_GUILD, [GuildId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Guild) -> [
        Guild#guild.guild_id,
        Guild#guild.exp,
        Guild#guild.wealth,
        Guild#guild.level,
        Guild#guild.create_time,
        Guild#guild.guild_name,
        Guild#guild.notice,
        Guild#guild.leader_id
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_GUILD, #guild.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD, []),
    Data = sql:select(Sql),
    parser:convert(Data, guild).

%% @doc update
update_notice(ThisNotice, GuildId) ->
    Sql = parser:format(?UPDATE_NOTICE, [ThisNotice, GuildId]),
    sql:update(Sql).

%% @doc update
update_name(ThisGuildName, GuildId) ->
    Sql = parser:format(?UPDATE_NAME, [ThisGuildName, GuildId]),
    sql:update(Sql).

%% @doc delete
delete_in_guild_id(GuildIdList) ->
    F = fun(GuildId) -> [GuildId] end,
    Sql = parser:collect(GuildIdList, F, ?DELETE_IN_GUILD_ID),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

