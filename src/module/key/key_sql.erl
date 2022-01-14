-module(key_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([select_by_key/1]).
-include("key.hrl").

-define(INSERT_KEY, <<"INSERT INTO `key` (`role_id`, `key`) VALUES (~i~w, '~s')">>).
-define(SELECT_KEY, <<"SELECT `role_id`, `key` FROM `key` WHERE `role_id` = ~w AND `key` = '~s'">>).
-define(UPDATE_KEY, {<<"UPDATE `key` SET ~i~i~i ">>, <<"WHERE `role_id` = ~w AND `key` = '~s'">>}).
-define(DELETE_KEY, <<"DELETE FROM `key` WHERE `role_id` = ~w AND `key` = '~s'">>).
-define(SELECT_BY_KEY, <<"SELECT `role_id`, `key` FROM `key` WHERE `key` = '~s'">>).
-define(SELECT_JOIN_BY_KEY, <<"SELECT `key`.`role_id`, `key`.`key` FROM `key` WHERE `key`.`key` = '~s'">>).

%% @doc insert
-spec insert(Key :: #key{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Key) ->
    Sql = parser:format(?INSERT_KEY, Key),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), Key :: binary()) -> KeyList :: [#key{}].
select(RoleId, Key) ->
    Sql = parser:format(?SELECT_KEY, [RoleId, Key]),
    Data = db:select(Sql),
    parser:convert(Data, key).

%% @doc update
-spec update(Key :: #key{}) -> AffectedRows :: non_neg_integer().
update(Key) ->
    Sql = <<(parser:format(element(1, ?UPDATE_KEY), Key))/binary, (parser:format(element(2, ?UPDATE_KEY), [Key#key.role_id, Key#key.key]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), Key :: binary()) -> AffectedRows :: non_neg_integer().
delete(RoleId, Key) ->
    Sql = parser:format(?DELETE_KEY, [RoleId, Key]),
    db:delete(Sql).

%% @doc select
-spec select_by_key(Key :: binary()) -> KeyList :: [#key{}].
select_by_key(Key) ->
    Sql = parser:format(?SELECT_BY_KEY, [Key]),
    Data = db:select(Sql),
    parser:convert(Data, key).

