-module(key_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("key.hrl").
-define(INSERT_KEY, <<"INSERT INTO `key` (`role_id`, `key`) VALUES (~w, '~s')">>).
-define(SELECT_KEY, <<"SELECT `role_id`, `key` FROM `key` WHERE `role_id` = ~w AND `key` = '~s'">>).
-define(UPDATE_KEY, <<"UPDATE `key` SET `role_id` = ~w, `key` = '~s' WHERE `role_id` = ~w AND `key` = '~s'">>).
-define(DELETE_KEY, <<"DELETE  FROM `key` WHERE `role_id` = ~w AND `key` = '~s'">>).
-define(SELECT_BY_KEY, <<"SELECT `role_id`, `key` FROM `key` WHERE `key` = '~s'">>).
-define(SELECT_JOIN_BY_KEY, <<"SELECT `key`.`role_id`, `key`.`key` FROM `key` WHERE `key`.`key` = '~s'">>).

%% @doc insert
insert(Key) ->
    Sql = parser:format(?INSERT_KEY, [
        Key#key.role_id,
        Key#key.key
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId, Key) ->
    Sql = parser:format(?SELECT_KEY, [RoleId, Key]),
    Data = sql:select(Sql),
    parser:convert(Data, key).

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

%% @doc select
select_by_key(Key) ->
    Sql = parser:format(?SELECT_BY_KEY, [Key]),
    Data = sql:select(Sql),
    parser:convert(Data, key).

