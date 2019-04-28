-module(rank_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("common.hrl").
-include("rank.hrl").

-define(UPDATE_INTO_RANK, {"INSERT INTO `rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `other`) VALUES ", "('~w', '~w', '~w', '~w', '~w', '~s', '~s')", " ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `other` = VALUES(`other`)"}).
-define(INSERT_RANK, "INSERT INTO `rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `other`) VALUES ('~w', '~w', '~w', '~w', '~w', '~s', '~s')").
-define(UPDATE_RANK, "UPDATE `rank` SET `key` = '~w', `value` = '~w', `time` = '~w', `name` = '~s', `other` = '~s' WHERE `type` = '~w' AND `rank` = '~w'").
-define(SELECT_RANK, "SELECT * FROM `rank` WHERE `type` = '~w'").
-define(DELETE_RANK, "DELETE * FROM `rank` WHERE `type` = '~w' AND `rank` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Rank) -> [
        Rank#rank.type,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.rank,
        Rank#rank.name,
        Rank#rank.other
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_RANK, #rank.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Rank) ->
    Sql = io_lib:format(?INSERT_RANK, [
        Rank#rank.type,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.rank,
        Rank#rank.name,
        Rank#rank.other
    ]),
    sql:insert(Sql).

%% @doc update
update(Rank) ->
    Sql = io_lib:format(?UPDATE_RANK, [
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.other,
        Rank#rank.type,
        Rank#rank.rank
    ]),
    sql:update(Sql).

%% @doc select
select(Type) ->
    Sql = io_lib:format(?SELECT_RANK, [
        Type
    ]),
    sql:select(Sql).

%% @doc delete
delete(Type, Rank) ->
    Sql = io_lib:format(?DELETE_RANK, [
        Type,
        Rank
    ]),
    sql:delete(Sql).

