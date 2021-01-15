-module(guild_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").
-define(INSERT_GUILD_ROLE, <<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w~i~i~i~i~i~i~i)">>).
-define(SELECT_GUILD_ROLE, <<"SELECT `guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`, '' AS `guild_name`, '' AS `role_name`, 0 AS `sex`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `flag` FROM `guild_role`">>).
-define(UPDATE_GUILD_ROLE, {<<"UPDATE `guild_role` SET ~i`guild_id` = ~w, ~i`job` = ~w, `wealth` = ~w, `join_time` = ~w, `leave_time` = ~w~i~i~i~i~i~i~i ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_GUILD_ROLE, <<"DELETE  FROM `guild_role` WHERE `role_id` = ~w">>).
-define(INSERT_UPDATE_GUILD_ROLE, {<<"INSERT INTO `guild_role` (`guild_id`, `role_id`, `job`, `wealth`, `join_time`, `leave_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w~i~i~i~i~i~i~i)">>, <<" ON DUPLICATE KEY UPDATE `guild_id` = VALUES(`guild_id`), `job` = VALUES(`job`), `wealth` = VALUES(`wealth`), `join_time` = VALUES(`join_time`), `leave_time` = VALUES(`leave_time`)">>}).
-define(SELECT_JOIN_GUILD_ROLE, <<"SELECT `guild_role`.`guild_id`, `guild_role`.`role_id`, `guild_role`.`job`, `guild_role`.`wealth`, `guild_role`.`join_time`, `guild_role`.`leave_time`, IFNULL(`guild`.`guild_name`, '') AS `guild_name`, IFNULL(`role`.`role_name`, '') AS `role_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`guild_role`.`flag`, 0) AS `flag` FROM `guild_role` LEFT JOIN `guild` ON `guild_role`.`guild_id` = `guild`.`guild_id` LEFT JOIN `role` ON `guild_role`.`role_id` = `role`.`role_id` LEFT JOIN `vip` ON `guild_role`.`role_id` = `vip`.`role_id`">>).

%% @doc insert
insert(GuildRole) ->
    Sql = parser:format(?INSERT_GUILD_ROLE, GuildRole),
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_GUILD_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_role).

%% @doc update
update(GuildRole) ->
    Sql = <<(parser:format(element(1, ?UPDATE_GUILD_ROLE), GuildRole))/binary, (parser:format(element(2, ?UPDATE_GUILD_ROLE), [GuildRole#guild_role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_GUILD_ROLE, [RoleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_GUILD_ROLE, #guild_role.flag),
    db:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_GUILD_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, guild_role).

