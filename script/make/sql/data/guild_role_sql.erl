-module(guild_role_sql).
-export([save/1]).
-export([select/0]).
-export([update_guild_id/1]).
-include("guild.hrl").

%% @doc insert into guild_role
-spec save(GuildRoleList :: [#guild_role{}] | ets:tab()) -> NewGuildRoleList :: [#guild_role{}].
save(GuildRoleList) ->
    db:save_into(<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:)">>, <<"ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `role_id` = VALUES(`role_id`), `job` = VALUES(`job`), `wealth` = VALUES(`wealth`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)">>, GuildRoleList, #guild_role.flag).

%% @doc select from guild_role
-spec select() -> Rows :: [#guild_role{}].
select() ->
    Data = db:select(<<"SELECT `guild_role`.`guild_id`, `guild_role`.`role_id`, `guild_role`.`job`, `guild_role`.`wealth`, `guild_role`.`join_time`, `guild_role`.`leave_time`, `guild`.`guild_name`, `role`.`role_name`, `role`.`sex`, `role`.`avatar`, `role`.`classes`, `role`.`level`, `vip`.`vip_level`, `guild_role`.`flag` FROM `guild_role` INNER JOIN `role` ON `role`.`role_id` = `guild_role`.`role_id` INNER JOIN `vip` ON `vip`.`role_id` = `guild_role`.`role_id` INNER JOIN `guild` ON `guild`.`guild_id` = `guild_role`.`guild_id`">>, []),
    parser:convert(Data, guild_role).

%% @doc update into guild_role
-spec update_guild_id(GuildRole :: #guild_role{}) -> AffectedRows :: non_neg_integer().
update_guild_id(GuildRole) ->
    db:update(<<"UPDATE `guild_role` SET `guild_id` = :2: WHERE `role_id` = :2:">>, GuildRole).
