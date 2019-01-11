-module(key_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("key.hrl").

-define(INSERT_KEY, "INSERT INTO `key` (`player_id`, `key`) VALUES ('~w', '~s')").
-define(UPDATE_KEY, "UPDATE `key` SET () VALUES () WHERE `player_id` = '~w' AND `key` = '~s'").
-define(SELECT_KEY, "SELECT * FROM `key` ").
-define(DELETE_KEY, "DELETE * FROM `key` WHERE `player_id` = '~w' AND `key` = '~s'").

%% @doc insert
insert(Key) ->
    Sql = io_lib:format(?INSERT_KEY, [
        Key#key.player_id,
        Key#key.key
    ]),
    sql:insert(?POOL, key, Sql).

%% @doc update
update(Key) ->
    Sql = io_lib:format(?UPDATE_KEY, [
        Key#key.player_id,
        Key#key.key
    ]),
    sql:update(?POOL, key, Sql).

%% @doc select
select() ->
    Sql = io_lib:format(?SELECT_KEY, [
        
    ]),
    sql:select(?POOL, key, Sql).

%% @doc delete
delete(PlayerId, Key) ->
    Sql = io_lib:format(?DELETE_KEY, [
        PlayerId,
        Key
    ]),
    sql:delete(?POOL, key, Sql).

