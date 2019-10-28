-module(key_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("key.hrl").
-define(INSERT_KEY, <<"INSERT INTO `key` (`role_id`, `key`) VALUES ('~w', '~w')">>).
-define(SELECT_KEY, <<"SELECT * FROM `key`">>).
-define(UPDATE_KEY, <<"UPDATE `key` SET `role_id` = '~w', `key` = '~w' WHERE `role_id` = '~w' AND `key` = '~w'">>).
-define(DELETE_KEY, <<"DELETE  FROM `key` WHERE `role_id` = '~w' AND `key` = '~w'">>).

%% @doc insert
insert(Key) ->
    Sql = parser:format(?INSERT_KEY, [
        Key#key.role_id,
        Key#key.key
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_KEY, []),
    sql:select(Sql).

%% @doc update
update(Key) ->
    Sql = parser:format(?UPDATE_KEY, [
        Key#key.role_id,
        Key#key.key,
        Key#key.role_id,
        Key#key.key
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, Key) ->
    Sql = parser:format(?DELETE_KEY, [RoleId, Key]),
    sql:delete(Sql).

