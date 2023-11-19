-module(guild_apply_sql).
-export([save/1]).
-export([select/0]).
-export([delete_by_guild_id/1]).
-export([delete_by_role_id/1]).
-export([delete/2]).
-include("guild.hrl").

%% @doc insert into guild_apply
-spec save(GuildApplyList :: [#guild_apply{}] | ets:tab()) -> NewGuildApplyList :: [#guild_apply{}].
save(GuildApplyList) ->
    db:save_into(<<"INSERT INTO `guild_apply` (`guild_id`, `role_id`, `apply_time`) VALUES">>, <<"(:2:, :3:, :4:)">>, <<"ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `role_id` = VALUES(`role_id`), `apply_time` = VALUES(`apply_time`)">>, GuildApplyList, #guild_apply.flag).

%% @doc select from guild_apply
-spec select() -> Rows :: [#guild_apply{}].
select() ->
    Data = db:select(<<"SELECT `guild_apply`.`guild_id`, `guild_apply`.`role_id`, `guild_apply`.`apply_time`, `guild`.`guild_name`, `role`.`role_name`, `role`.`sex`, `role`.`avatar`, `role`.`classes`, `role`.`level`, `vip`.`vip_level`, `guild_apply`.`flag` FROM `guild_apply` INNER JOIN `role` ON `role`.`role_id` = `guild_apply`.`role_id` INNER JOIN `vip` ON `vip`.`role_id` = `guild_apply`.`role_id` INNER JOIN `guild` ON `guild`.`guild_id` = `guild_apply`.`guild_id`">>, []),
    parser:convert(Data, guild_apply).

%% @doc delete row from guild_apply
-spec delete_by_guild_id(GuildId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_guild_id(GuildId) ->
    db:delete(<<"DELETE FROM `guild_apply` WHERE `guild_id` = ?">>, [GuildId]).

%% @doc delete row from guild_apply
-spec delete_by_role_id(RoleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_role_id(RoleId) ->
    db:delete(<<"DELETE FROM `guild_apply` WHERE `role_id` = ?">>, [RoleId]).

%% @doc delete row from guild_apply
-spec delete(RoleId :: non_neg_integer(), GuildId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, GuildId) ->
    db:delete(<<"DELETE FROM `guild_apply` WHERE `role_id` = ? AND `guild_id` = ?">>, [RoleId, GuildId]).
