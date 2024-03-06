-module(guild_sql).
-export([save/1]).
-export([select/0]).
-export([update_name/1]).
-export([update_notice/1]).
-export([delete/1]).
-include("guild.hrl").

%% @doc insert into guild
-spec save(GuildList :: [#guild{}] | ets:tab()) -> NewGuildList :: [#guild{}].
save(GuildList) ->
    db:save_into(<<"INSERT INTO `guild` (`guild_id`, `guild_name`, `exp`, `wealth`, `level`, `create_time`, `notice`, `leader_role_id`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?)">>, <<"ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `exp` = VALUES(`exp`), `wealth` = VALUES(`wealth`), `level` = VALUES(`level`), `create_time` = VALUES(`create_time`), `notice` = VALUES(`notice`), `leader_role_id` = VALUES(`leader_role_id`)">>, GuildList, fun(#guild{guild_id = GuildId, guild_name = GuildName, exp = Exp, wealth = Wealth, level = Level, create_time = CreateTime, notice = Notice, leader_role_id = LeaderRoleId}) -> [GuildId, GuildName, Exp, Wealth, Level, CreateTime, Notice, LeaderRoleId] end, #guild.flag).

%% @doc select from guild
-spec select() -> Rows :: [#guild{}].
select() ->
    Data = db:select(<<"SELECT `guild`.`guild_id`, `guild`.`guild_name`, `guild`.`exp`, `guild`.`wealth`, `guild`.`level`, `guild`.`create_time`, `guild`.`notice`, `guild`.`leader_role_id`, `role`.`role_name`, `role`.`sex`, `role`.`avatar`, `role`.`classes`, `role`.`level`, `vip`.`vip_level`, `guild`.`flag` FROM `guild` INNER JOIN `role` ON `role`.`role_id` = `guild`.`leader_role_id` INNER JOIN `vip` ON `vip`.`role_id` = `guild`.`leader_role_id`">>, []),
    parser:convert(Data, guild).

%% @doc update into guild
-spec update_name(#guild{}) -> AffectedRows :: non_neg_integer().
update_name(#guild{guild_name = GuildName, guild_id = GuildId}) ->
    db:update(<<"UPDATE `guild` SET `guild_name` = ? WHERE `guild_id` = ?">>, [GuildName, GuildId]).

%% @doc update into guild
-spec update_notice(#guild{}) -> AffectedRows :: non_neg_integer().
update_notice(#guild{notice = Notice, guild_id = GuildId}) ->
    db:update(<<"UPDATE `guild` SET `notice` = ? WHERE `guild_id` = ?">>, [Notice, GuildId]).

%% @doc delete row from guild
-spec delete(GuildId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(GuildId) ->
    db:delete(<<"DELETE FROM `guild` WHERE `guild_id` = ?">>, [GuildId]).
