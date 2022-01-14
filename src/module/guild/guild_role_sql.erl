-module(guild_role_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/1]).
-export([insert_update/1]).
-export([select_join/0]).
-include("guild.hrl").

-define(INSERT_GUILD_ROLE, <<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w~i~i~i~i~i~i~i~i)">>).
-define(SELECT_GUILD_ROLE, <<"SELECT `guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`, '' AS `guild_name`, '' AS `role_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `flag` FROM `guild_role`">>).
-define(UPDATE_GUILD_ROLE, {<<"UPDATE `guild_role` SET ~i`guild_id` = ~w, ~i`job` = ~w, `wealth` = ~w, `join_time` = ~w, `leave_time` = ~w~i~i~i~i~i~i~i~i ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_GUILD_ROLE, <<"DELETE FROM `guild_role` WHERE `role_id` = ~w">>).
-define(INSERT_UPDATE_GUILD_ROLE, {<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i~i~i~i~i~i~i~i)">>, <<" ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `job` = VALUES(`job`), `wealth` = VALUES(`wealth`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)">>}).
-define(SELECT_JOIN_GUILD_ROLE, <<"SELECT `guild_role`.`guild_id`, `guild_role`.`role_id`, `guild_role`.`job`, `guild_role`.`wealth`, `guild_role`.`join_time`, `guild_role`.`leave_time`, IFNULL(`guild`.`guild_name`, '') AS `guild_name`, IFNULL(`role`.`role_name`, '') AS `role_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`guild_role`.`flag`, 0) AS `flag` FROM `guild_role` LEFT JOIN `guild` ON `guild_role`.`guild_id` = `guild`.`guild_id` LEFT JOIN `role` ON `guild_role`.`role_id` = `role`.`role_id` LEFT JOIN `vip` ON `guild_role`.`role_id` = `vip`.`role_id`">>).

%% @doc insert
-spec insert(GuildRole :: #guild_role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(GuildRole) ->
    Sql = parser:format(?INSERT_GUILD_ROLE, GuildRole),
    db:insert(Sql).

%% @doc select
-spec select() -> GuildRoleList :: [#guild_role{}].
select() ->
    Sql = parser:format(?SELECT_GUILD_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_role).

%% @doc update
-spec update(GuildRole :: #guild_role{}) -> AffectedRows :: non_neg_integer().
update(GuildRole) ->
    Sql = <<(parser:format(element(1, ?UPDATE_GUILD_ROLE), GuildRole))/binary, (parser:format(element(2, ?UPDATE_GUILD_ROLE), [GuildRole#guild_role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId) ->
    Sql = parser:format(?DELETE_GUILD_ROLE, [RoleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(GuildRoleList :: [#guild_role{}] | ets:tab()) -> NewGuildRoleList :: [#guild_role{}].
insert_update(GuildRoleList) ->
    {Sql, NewGuildRoleList} = parser:collect_into(GuildRoleList, ?INSERT_UPDATE_GUILD_ROLE, #guild_role.flag),
    db:insert(Sql),
    NewGuildRoleList.

%% @doc select join
-spec select_join() -> GuildRoleList :: [#guild_role{}].
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_role).

