-module(guild_request_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").

-define(SELECT_JOIN_GUILD_REQUEST, "SELECT IFNULL(`player`.`id`, 0), `guild_request`.`guild_id`, `guild_request`.`time`, IFNULL(`player`.`name`, ''), `guild_request`.`player_pid`, `guild_request`.`sender_pid`, IFNULL(`player`.`server_id`, ''), `guild_request`.`extra`, `guild_request`.`flag` FROM `guild_request`  LEFT JOIN `player` ON `guild_request`.`player_id` = `player`.`id` ").
-define(UPDATE_INTO_GUILD_REQUEST, {"INSERT INTO `guild_request` (`player_id`, `guild_id`, `time`) VALUES ", "('~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `time` = VALUES(`time`)"}).
-define(INSERT_GUILD_REQUEST, "INSERT INTO `guild_request` (`player_id`, `guild_id`, `time`) VALUES ('~w', '~w', '~w')").
-define(UPDATE_GUILD_REQUEST, "UPDATE `guild_request` SET `time` = '~w' WHERE `player_id` = '~w' AND `guild_id` = '~w'").
-define(SELECT_GUILD_REQUEST, "SELECT * FROM `guild_request` ").
-define(DELETE_GUILD_REQUEST, "DELETE  FROM `guild_request` WHERE `player_id` = '~w' AND `guild_id` = '~w'").
-define(DELETE_GUILD_REQUEST_PLAYER, "DELETE  FROM `guild_request` WHERE `player_id` = '~w'").
-define(DELETE_GUILD_REQUEST_GUILD, "DELETE  FROM `guild_request` WHERE `guild_id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(GuildRequest) -> [
        GuildRequest#guild_request.player_id,
        GuildRequest#guild_request.guild_id,
        GuildRequest#guild_request.time
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_GUILD_REQUEST, #guild_request.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(GuildRequest) ->
    Sql = io_lib:format(?INSERT_GUILD_REQUEST, [
        GuildRequest#guild_request.player_id,
        GuildRequest#guild_request.guild_id,
        GuildRequest#guild_request.time
    ]),
    sql:insert(Sql).

%% @doc update
update(GuildRequest) ->
    Sql = io_lib:format(?UPDATE_GUILD_REQUEST, [
        GuildRequest#guild_request.time,
        GuildRequest#guild_request.player_id,
        GuildRequest#guild_request.guild_id
    ]),
    sql:update(Sql).

%% @doc select
select() ->
    Sql = io_lib:format(?SELECT_GUILD_REQUEST, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete(PlayerId, GuildId) ->
    Sql = io_lib:format(?DELETE_GUILD_REQUEST, [
        PlayerId,
        GuildId
    ]),
    sql:delete(Sql).

%% @doc select join
select_join() ->
    Sql = io_lib:format(?SELECT_JOIN_GUILD_REQUEST, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete_player(PlayerId) ->
    Sql = io_lib:format(?DELETE_GUILD_REQUEST_PLAYER, [
        PlayerId
    ]),
    sql:delete(Sql).

%% @doc delete
delete_guild(GuildId) ->
    Sql = io_lib:format(?DELETE_GUILD_REQUEST_GUILD, [
        GuildId
    ]),
    sql:delete(Sql).

