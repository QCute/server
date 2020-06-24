-module(rank_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("rank.hrl").
-define(INSERT_RANK, <<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES (~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w')">>).
-define(SELECT_RANK, <<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, `flag` FROM `rank` WHERE `type` = ~w">>).
-define(UPDATE_RANK, <<"UPDATE `rank` SET `key` = ~w, `value` = ~w, `time` = ~w, `name` = '~s', `server_id` = ~w, `digest` = '~w', `extra` = '~w', `other` = '~w' WHERE `type` = ~w AND `order` = ~w">>).
-define(DELETE_RANK, <<"DELETE  FROM `rank` WHERE `type` = ~w AND `order` = ~w">>).
-define(INSERT_UPDATE_RANK, {<<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `server_id` = VALUES(`server_id`), `digest` = VALUES(`digest`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)">>}).
-define(DELETE_TYPE, <<"DELETE FROM `rank` WHERE `type` = ~w">>).
-define(TRUNCATE, <<"TRUNCATE TABLE `rank`">>).

%% @doc insert
insert(Rank) ->
    Sql = parser:format(?INSERT_RANK, [
        Rank#rank.type,
        Rank#rank.order,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.server_id,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other
    ]),
    sql:insert(Sql).

%% @doc select
select(Type) ->
    Sql = parser:format(?SELECT_RANK, [Type]),
    Data = sql:select(Sql),
    F = fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end,
    parser:convert(Data, rank, F).

%% @doc update
update(Rank) ->
    Sql = parser:format(?UPDATE_RANK, [
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.server_id,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other,
        Rank#rank.type,
        Rank#rank.order
    ]),
    sql:update(Sql).

%% @doc delete
delete(Type, Order) ->
    Sql = parser:format(?DELETE_RANK, [Type, Order]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Rank) -> [
        Rank#rank.type,
        Rank#rank.order,
        Rank#rank.key,
        Rank#rank.value,
        Rank#rank.time,
        Rank#rank.name,
        Rank#rank.server_id,
        Rank#rank.digest,
        Rank#rank.extra,
        Rank#rank.other
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_RANK, #rank.flag),
    sql:insert(Sql),
    NewData.

%% @doc delete
delete_type(Type) ->
    Sql = parser:format(?DELETE_TYPE, [Type]),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

