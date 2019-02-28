-module(player_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("player.hrl").

-define(INSERT_PLAYER, "INSERT INTO `player` (`account`, `name`, `sex`, `level`, `classes`, `focus`) VALUES ('~s', '~s', '~w', '~w', '~w', '~w')").
-define(UPDATE_PLAYER, "UPDATE `player` SET `sex` = '~w', `level` = '~w', `classes` = '~w', `focus` = '~w' WHERE `id` = '~w'").
-define(SELECT_PLAYER, "SELECT * FROM `player` WHERE `id` = '~w'").
-define(DELETE_PLAYER, "DELETE * FROM `player` WHERE `id` = '~w'").

%% @doc insert
insert(Player) ->
    Sql = io_lib:format(?INSERT_PLAYER, [
        Player#player.account,
        Player#player.name,
        Player#player.sex,
        Player#player.level,
        Player#player.classes,
        Player#player.focus
    ]),
    sql:insert(Sql).

%% @doc update
update(Player) ->
    Sql = io_lib:format(?UPDATE_PLAYER, [
        Player#player.sex,
        Player#player.level,
        Player#player.classes,
        Player#player.focus,
        Player#player.id
    ]),
    sql:update(Sql).

%% @doc select
select(Id) ->
    Sql = io_lib:format(?SELECT_PLAYER, [
        Id
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_PLAYER, [
        Id
    ]),
    sql:delete(Sql).

