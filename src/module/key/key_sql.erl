-module(key_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("key.hrl").

-define(INSERT_KEY, "INSERT INTO `key` (`role_id`, `key`) VALUES ('~w', '~s')").
-define(UPDATE_KEY, "UPDATE `key` SET `role_id` = '~w', `key` = '~s' WHERE `role_id` = '~w' AND `key` = '~s'").
-define(SELECT_KEY, "SELECT * FROM `key` ").
-define(DELETE_KEY, "DELETE  FROM `key` WHERE `role_id` = '~w' AND `key` = '~s'").

%% @doc insert
insert(Key) ->
    Sql = io_lib:format(?INSERT_KEY, [
        Key#key.role_id,
        Key#key.key
    ]),
    sql:insert(Sql).

%% @doc update
update(Key) ->
    Sql = io_lib:format(?UPDATE_KEY, [
        Key#key.role_id,
        Key#key.key,
        Key#key.role_id,
        Key#key.key
    ]),
    sql:update(Sql).

%% @doc select
select() ->
    Sql = io_lib:format(?SELECT_KEY, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId, Key) ->
    Sql = io_lib:format(?DELETE_KEY, [
        RoleId,
        Key
    ]),
    sql:delete(Sql).

