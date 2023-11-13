-module(key_sql).
-export([insert/1]).
-export([select/1]).
-include("key.hrl").

%% @doc insert into key
-spec insert(Key :: #key{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Key) ->
    db:insert(<<"INSERT INTO `key` (`role_id`, `key`) VALUES (:2:, :3:)">>, Key).

%% @doc select from key
-spec select(Key :: binary()) -> Rows :: [#key{}].
select(Key) ->
    Data = db:select(<<"SELECT `role_id`, `key` FROM `key` WHERE `key` = ?">>, [Key]),
    parser:convert(Data, key).
