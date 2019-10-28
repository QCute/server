-module(rank_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("rank.hrl").
-define(INSERT_RANK, <<"INSERT INTO `rank` (`type`, `rank`, `key`, `value`, `time`, `name`, `digest`, `extra`, `other`) VALUES ('~w', '~w', '~w', '~w', '~w', '~s', '~w', '~w', '~w')">>).
-define(SELECT_RANK, <<"SELECT * FROM `rank` WHERE `type` = '~w'">>).
-define(UPDATE_RANK, <<"UPDATE `rank` SET `key` = '~w', `value` = '~w', `time` = '~w', `name` = '~s', `digest` = '~w', `extra` = '~w', `other` = '~w' WHERE `type` = '~w' AND `rank` = '~w'">>).
-define(DELETE_RANK, <<"DELETE  FROM `rank` WHERE `type` = '~w' AND `rank` = '~w'">>).
-define(INSERT_UPDATE_RANK, {<<"INSERT INTO `rank` (`type`, `rank`, `key`, `value`, `time`, `name`, `digest`, `extra`, `other`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~s', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `digest` = VALUES(`digest`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)">>}).

%% @doc insert
insert(Rank) ->
    Sql = parser:format(?INSERT_RANK, [
        Rank#rank.type,
        Rank#rank.rank,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other
    ]),
    sql:insert(Sql).

%% @doc select
select(Type) ->
    Sql = parser:format(?SELECT_RANK, [Type]),
    sql:select(Sql).

%% @doc update
update(Rank) ->
    Sql = parser:format(?UPDATE_RANK, [
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other,
        Rank#rank.type,
        Rank#rank.rank
    ]),
    sql:update(Sql).

%% @doc delete
delete(Type, Rank) ->
    Sql = parser:format(?DELETE_RANK, [Type, Rank]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Rank) -> [
        Rank#rank.type,
        Rank#rank.rank,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_RANK, #rank.flag),
    sql:insert(Sql),
    NewData.

