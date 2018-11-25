-module(player_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("player.hrl").

-define(INSERT_PLAYER, <<"INSERT INTO `player` (`name`, `nick`, `sex`, `focus`) VALUES ('~s', '~s', '~w', '~w')">>).
-define(UPDATE_PLAYER, <<"UPDATE `player` SET (`sex`, `focus`) VALUES ('~w', '~w') WHERE `id` = '~w'">>).
-define(SELECT_PLAYER, <<"SELECT * FROM `player` WHERE `id` = '~w'">>).
-define(DELETE_PLAYER, <<"DELETE * FROM `player` WHERE `id` = '~w'">>).

%% @doc insert
insert(Player) ->
    Sql = io_lib:format(?INSERT_PLAYER, [
        Player#player.name,
        Player#player.nick,
        Player#player.sex,
        Player#player.focus
    ]),
    sql:execute(?DB_GAME, player, Sql).

%% @doc update
update(Player) ->
    Sql = io_lib:format(?UPDATE_PLAYER, [
        Player#player.sex,
        Player#player.focus,
        Player#player.id
    ]),
    sql:execute(?DB_GAME, player, Sql).

%% @doc select
select(Id) ->
    Sql = io_lib:format(?SELECT_PLAYER, [
        Id
    ]),
    sql:execute(?DB_GAME, player, Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_PLAYER, [
        Id
    ]),
    sql:execute(?DB_GAME, player, Sql).

