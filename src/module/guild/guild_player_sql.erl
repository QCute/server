-module(guild_player_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("guild.hrl").

-define(SELECT_JOIN_GUILD_PLAYER, "SELECT `guild`.`guild_id`, `player`.`id`, `guild_player`.`job`, `guild_player`.`join_time`, `guild_player`.`leave_time`, `guild`.`guild_name`, `player`.`name`, `player`.`nick`, `guild_player`.`extra` FROM `guild_player`  LEFT JOIN `guild` ON `guild_player`.`guild_id` = `guild`.`guild_id` LEFT JOIN `player` ON `guild_player`.`player_id` = `player`.`id` ").
-define(UPDATE_INTO_GUILD_PLAYER, {"INSERT INTO `guild_player` (`guild_id`, `player_id`, `job`, `join_time`, `leave_time`) VALUES ", "('~w', '~w', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `job` = VALUES(`job`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)"}).
-define(INSERT_GUILD_PLAYER, "INSERT INTO `guild_player` (`guild_id`, `player_id`, `job`, `join_time`, `leave_time`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_GUILD_PLAYER, "UPDATE `guild_player` SET (`job`, `join_time`, `leave_time`) VALUES ('~w', '~w', '~w') WHERE `guild_id` = '~w' AND `player_id` = '~w'").
-define(SELECT_GUILD_PLAYER, "SELECT * FROM `guild_player` ").
-define(DELETE_GUILD_PLAYER, "DELETE * FROM `guild_player` WHERE `guild_id` = '~w' AND `player_id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(GuildPlayer) -> [
        GuildPlayer#guild_player.guild_id,
        GuildPlayer#guild_player.player_id,
        GuildPlayer#guild_player.job,
        GuildPlayer#guild_player.join_time,
        GuildPlayer#guild_player.leave_time
    ] end,
    {Sql, NewData} = data_tool:collect(DataList, F, ?UPDATE_INTO_GUILD_PLAYER, #guild_player.extra),
    sql:insert(?POOL, guild_player, Sql),
    NewData.


%% @doc insert
insert(GuildPlayer) ->
    Sql = io_lib:format(?INSERT_GUILD_PLAYER, [
        GuildPlayer#guild_player.guild_id,
        GuildPlayer#guild_player.player_id,
        GuildPlayer#guild_player.job,
        GuildPlayer#guild_player.join_time,
        GuildPlayer#guild_player.leave_time
    ]),
    sql:insert(?POOL, guild_player, Sql).

%% @doc update
update(GuildPlayer) ->
    Sql = io_lib:format(?UPDATE_GUILD_PLAYER, [
        GuildPlayer#guild_player.job,
        GuildPlayer#guild_player.join_time,
        GuildPlayer#guild_player.leave_time,
        GuildPlayer#guild_player.guild_id,
        GuildPlayer#guild_player.player_id
    ]),
    sql:update(?POOL, guild_player, Sql).

%% @doc select
select() ->
    Sql = io_lib:format(?SELECT_GUILD_PLAYER, [
        
    ]),
    sql:select(?POOL, guild_player, Sql).

%% @doc delete
delete(GuildId, PlayerId) ->
    Sql = io_lib:format(?DELETE_GUILD_PLAYER, [
        GuildId,
        PlayerId
    ]),
    sql:delete(?POOL, guild_player, Sql).

%% @doc select join
select_join() ->
    Sql = io_lib:format(?SELECT_JOIN_GUILD_PLAYER, [
        
    ]),
    sql:select(?POOL, guild_player, Sql).

