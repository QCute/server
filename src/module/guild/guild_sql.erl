-module(guild_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD, <<"INSERT INTO `guild` (`exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`) VALUES (~w, ~w, ~w, ~w, '~s', '~s', ~w)">>).
-define(SELECT_GUILD, <<"SELECT `guild_id`, `exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`, '' AS `leader_name`, 0 AS `leader_sex`, 0 AS `leader_class`, 0 AS `leader_level`, 0 AS `leader_vip_level`, 0 AS `flag` FROM `guild`">>).
-define(UPDATE_GUILD, <<"UPDATE `guild` SET `exp` = ~w, `wealth` = ~w, `level` = ~w, `create_time` = ~w, `guild_name` = '~s', `notice` = '~s', `leader_id` = ~w WHERE `guild_id` = ~w">>).
-define(DELETE_GUILD, <<"DELETE  FROM `guild` WHERE `guild_id` = ~w">>).
-define(INSERT_UPDATE_GUILD, {<<"INSERT INTO `guild` (`guild_id`, `exp`, `wealth`, `level`, `create_time`, `guild_name`, `notice`, `leader_id`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, '~s', '~s', ~w)">>, <<" ON DUPLICATE KEY UPDATE `exp` = VALUES(`exp`), `wealth` = VALUES(`wealth`), `level` = VALUES(`level`), `create_time` = VALUES(`create_time`), `guild_name` = VALUES(`guild_name`), `notice` = VALUES(`notice`), `leader_id` = VALUES(`leader_id`)">>}).
-define(SELECT_JOIN_GUILD, <<"SELECT `guild`.`guild_id`, `guild`.`exp`, `guild`.`wealth`, `guild`.`level`, `guild`.`create_time`, `guild`.`guild_name`, `guild`.`notice`, `guild`.`leader_id`, IFNULL(`role`.`role_name`, '') AS `leader_name`, IFNULL(`role`.`sex`, 0) AS `leader_sex`, IFNULL(`role`.`classes`, 0) AS `leader_class`, IFNULL(`role`.`level`, 0) AS `leader_level`, IFNULL(`vip`.`vip_level`, 0) AS `leader_vip_level`, IFNULL(`guild`.`flag`, 0) AS `flag` FROM `guild` LEFT JOIN `role` ON `guild`.`leader_id` = `role`.`role_id` LEFT JOIN `vip` ON `guild`.`leader_id` = `vip`.`role_id`">>).
-define(UPDATE_NAME, <<"UPDATE `guild` SET `guild_name` = '~s' WHERE `guild_id` = ~w">>).
-define(UPDATE_NOTICE, <<"UPDATE `guild` SET `notice` = '~s' WHERE `guild_id` = ~w">>).
-define(DELETE_IN_GUILD_ID, {<<"DELETE  FROM `guild` WHERE `guild_id` in (">>, <<"~w">>, <<")">>}).

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
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD, []),
    Data = db:select(Sql),
    parser:convert(Data, guild).

%% @doc update
update(Guild) ->
    Sql = parser:format(?UPDATE_GUILD, [
        Guild#guild.exp,
        Guild#guild.wealth,
        Guild#guild.level,
        Guild#guild.create_time,
        Guild#guild.guild_name,
        Guild#guild.notice,
        Guild#guild.leader_id,
        Guild#guild.guild_id
    ]),
    db:update(Sql).

%% @doc delete
delete(GuildId) ->
    Sql = parser:format(?DELETE_GUILD, [GuildId]),
    db:delete(Sql).


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
    db:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD, []),
    Data = db:select(Sql),
    parser:convert(Data, guild).

%% @doc update
update_name(ThisGuildName, GuildId) ->
    Sql = parser:format(?UPDATE_NAME, [ThisGuildName, GuildId]),
    db:update(Sql).

%% @doc update
update_notice(ThisNotice, GuildId) ->
    Sql = parser:format(?UPDATE_NOTICE, [ThisNotice, GuildId]),
    db:update(Sql).

%% @doc delete
delete_in_guild_id(GuildIdList) ->
    F = fun(GuildId) -> [GuildId] end,
    Sql = parser:collect(GuildIdList, F, ?DELETE_IN_GUILD_ID),
    db:delete(Sql).

