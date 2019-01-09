-module(key_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("key.hrl").

-define(UPDATE_INTO_KEY, {"INSERT INTO `key` (`player_id`, `key`) VALUES ", "('~w', '~w')", " ON DUPLICATE KEY UPDATE "}).
-define(INSERT_KEY, "INSERT INTO `key` (`player_id`, `key`) VALUES ('~w', '~w')").
-define(UPDATE_KEY, "UPDATE `key` SET () VALUES () WHERE `player_id` = '~w' AND `key` = '~w'").
-define(SELECT_KEY, "SELECT * FROM `key` WHERE `player_id` = '~w'").
-define(DELETE_KEY, "DELETE * FROM `key` WHERE `player_id` = '~w' AND `key` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Key) -> [
        Key#key.player_id,
        Key#key.key
    ] end,
    {Sql, NewData} = data_tool:collect(DataList, F, ?UPDATE_INTO_KEY, #key.extra),
    sql:insert(?POOL, key, Sql),
    NewData.


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
select(PlayerId) ->
    Sql = io_lib:format(?SELECT_KEY, [
        PlayerId
    ]),
    sql:select(?POOL, key, Sql).

%% @doc delete
delete(PlayerId, Key) ->
    Sql = io_lib:format(?DELETE_KEY, [
        PlayerId,
        Key
    ]),
    sql:delete(?POOL, key, Sql).

