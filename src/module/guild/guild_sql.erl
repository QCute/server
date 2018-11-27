-module(guild_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("guild.hrl").

-define(UPDATE_INTO_GUILD, {"INSERT INTO `guild` (`id`, `name`, `create_time`, `exp`, `wealth`) VALUES ", "('~w', '~s', '~w', '~w', '~w')", " ON DUPLICATE KEY UPDATE `name` = VALUES(`name`), `create_time` = VALUES(`create_time`), `exp` = VALUES(`exp`), `wealth` = VALUES(`wealth`)"}).
-define(INSERT_GUILD, "INSERT INTO `guild` (`name`, `create_time`, `exp`, `wealth`) VALUES ('~s', '~w', '~w', '~w')").
-define(UPDATE_GUILD, "UPDATE `guild` SET (`name`, `create_time`, `exp`, `wealth`) VALUES ('~s', '~w', '~w', '~w') WHERE `id` = '~w'").
-define(SELECT_GUILD, "SELECT * FROM `guild` WHERE `id` = '~w'").
-define(DELETE_GUILD, "DELETE * FROM `guild` WHERE `id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Guild) -> [
        Guild#guild.id,
        Guild#guild.name,
        Guild#guild.create_time,
        Guild#guild.exp,
        Guild#guild.wealth
    ] end,
    {Sql, NewData} = data_tool:collect(DataList, F, ?UPDATE_INTO_GUILD, #guild.extra),
    sql:execute(?POOL, guild, Sql),
    NewData.


%% @doc insert
insert(Guild) ->
    Sql = io_lib:format(?INSERT_GUILD, [
        Guild#guild.name,
        Guild#guild.create_time,
        Guild#guild.exp,
        Guild#guild.wealth
    ]),
    sql:execute(?POOL, guild, Sql).

%% @doc update
update(Guild) ->
    Sql = io_lib:format(?UPDATE_GUILD, [
        Guild#guild.name,
        Guild#guild.create_time,
        Guild#guild.exp,
        Guild#guild.wealth,
        Guild#guild.id
    ]),
    sql:execute(?POOL, guild, Sql).

%% @doc select
select(Id) ->
    Sql = io_lib:format(?SELECT_GUILD, [
        Id
    ]),
    sql:execute(?POOL, guild, Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_GUILD, [
        Id
    ]),
    sql:execute(?POOL, guild, Sql).

