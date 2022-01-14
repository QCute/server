-module(guild_apply_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_join/0]).
-export([delete_by_guild_id/1]).
-export([delete_by_role_id/1]).
-include("guild.hrl").

-define(INSERT_GUILD_APPLY, <<"INSERT INTO `guild_apply` (`guild_id`, `role_id`, `apply_time`) VALUES (~i~w, ~w, ~w~i~i~i~i~i~i~i~i)">>).
-define(SELECT_GUILD_APPLY, <<"SELECT `guild_id`, `role_id`, `apply_time`, '' AS `guild_name`, '' AS `role_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `flag` FROM `guild_apply`">>).
-define(UPDATE_GUILD_APPLY, {<<"UPDATE `guild_apply` SET ~i~i~i`apply_time` = ~w~i~i~i~i~i~i~i~i ">>, <<"WHERE `guild_id` = ~w AND `role_id` = ~w">>}).
-define(DELETE_GUILD_APPLY, <<"DELETE FROM `guild_apply` WHERE `guild_id` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_GUILD_APPLY, {<<"INSERT INTO `guild_apply` (`guild_id`, `role_id`, `apply_time`) VALUES ">>, <<"(~i~w, ~w, ~w~i~i~i~i~i~i~i~i)">>, <<" ON DUPLICATE KEY UPDATE `apply_time` = VALUES(`apply_time`)">>}).
-define(SELECT_JOIN_GUILD_APPLY, <<"SELECT `guild_apply`.`guild_id`, `guild_apply`.`role_id`, `guild_apply`.`apply_time`, IFNULL(`guild`.`guild_name`, '') AS `guild_name`, IFNULL(`role`.`role_name`, '') AS `role_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`guild_apply`.`flag`, 0) AS `flag` FROM `guild_apply` LEFT JOIN `guild` ON `guild_apply`.`guild_id` = `guild`.`guild_id` LEFT JOIN `role` ON `guild_apply`.`role_id` = `role`.`role_id` LEFT JOIN `vip` ON `guild_apply`.`role_id` = `vip`.`role_id`">>).
-define(DELETE_BY_GUILD_ID, <<"DELETE FROM `guild_apply` WHERE `guild_id` = ~w">>).
-define(DELETE_BY_ROLE_ID, <<"DELETE FROM `guild_apply` WHERE `role_id` = ~w">>).

%% @doc insert
-spec insert(GuildApply :: #guild_apply{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(GuildApply) ->
    Sql = parser:format(?INSERT_GUILD_APPLY, GuildApply),
    db:insert(Sql).

%% @doc select
-spec select() -> GuildApplyList :: [#guild_apply{}].
select() ->
    Sql = parser:format(?SELECT_GUILD_APPLY, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_apply).

%% @doc update
-spec update(GuildApply :: #guild_apply{}) -> AffectedRows :: non_neg_integer().
update(GuildApply) ->
    Sql = <<(parser:format(element(1, ?UPDATE_GUILD_APPLY), GuildApply))/binary, (parser:format(element(2, ?UPDATE_GUILD_APPLY), [GuildApply#guild_apply.guild_id, GuildApply#guild_apply.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(GuildId :: integer(), RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(GuildId, RoleId) ->
    Sql = parser:format(?DELETE_GUILD_APPLY, [GuildId, RoleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(GuildApplyList :: [#guild_apply{}] | ets:tab()) -> NewGuildApplyList :: [#guild_apply{}].
insert_update(GuildApplyList) ->
    {Sql, NewGuildApplyList} = parser:collect_into(GuildApplyList, ?INSERT_UPDATE_GUILD_APPLY, #guild_apply.flag),
    db:insert(Sql),
    NewGuildApplyList.

%% @doc select join
-spec select_join() -> GuildApplyList :: [#guild_apply{}].
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_APPLY, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_apply).

%% @doc delete
-spec delete_by_guild_id(GuildId :: integer()) -> AffectedRows :: non_neg_integer().
delete_by_guild_id(GuildId) ->
    Sql = parser:format(?DELETE_BY_GUILD_ID, [GuildId]),
    db:delete(Sql).

%% @doc delete
-spec delete_by_role_id(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete_by_role_id(RoleId) ->
    Sql = parser:format(?DELETE_BY_ROLE_ID, [RoleId]),
    db:delete(Sql).

