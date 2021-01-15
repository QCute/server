-module(rank_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("rank.hrl").
-define(INSERT_RANK, <<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES (~i~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w'~i)">>).
-define(SELECT_RANK, <<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, 1 AS `flag` FROM `rank` WHERE `type` = ~w AND `order` = ~w">>).
-define(UPDATE_RANK, {<<"UPDATE `rank` SET ~i~i~i`key` = ~w, `value` = ~w, `time` = ~w, `name` = '~s', `server_id` = ~w, `digest` = '~w', `extra` = '~w', `other` = '~w'~i ">>, <<"WHERE `type` = ~w AND `order` = ~w">>}).
-define(DELETE_RANK, <<"DELETE  FROM `rank` WHERE `type` = ~w AND `order` = ~w">>).
-define(INSERT_UPDATE_RANK, {<<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w'~i)">>, <<" ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `server_id` = VALUES(`server_id`), `digest` = VALUES(`digest`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)">>}).
-define(SELECT_BY_TYPE, <<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, 1 AS `flag` FROM `rank` WHERE `type` = ~w">>).
-define(SELECT_JOIN_BY_TYPE, <<"SELECT `rank`.`type`, `rank`.`order`, `rank`.`key`, `rank`.`value`, `rank`.`time`, `rank`.`name`, `rank`.`server_id`, `rank`.`digest`, `rank`.`extra`, `rank`.`other`, IFNULL(`rank`.`flag`, 1) AS `flag` FROM `rank` WHERE `rank`.`type` = ~w">>).
-define(DELETE_BY_TYPE, <<"DELETE FROM `rank` WHERE `type` = ~w">>).

%% @doc insert
insert(Rank) ->
    Sql = parser:format(?INSERT_RANK, Rank),
    db:insert(Sql).

%% @doc select
select(Type, Order) ->
    Sql = parser:format(?SELECT_RANK, [Type, Order]),
    Data = db:select(Sql),
    F = fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end,
    parser:convert(Data, rank, F).

%% @doc update
update(Rank) ->
    Sql = <<(parser:format(element(1, ?UPDATE_RANK), Rank))/binary, (parser:format(element(2, ?UPDATE_RANK), [Rank#rank.type, Rank#rank.order]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(Type, Order) ->
    Sql = parser:format(?DELETE_RANK, [Type, Order]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_RANK, #rank.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_type(Type) ->
    Sql = parser:format(?SELECT_BY_TYPE, [Type]),
    Data = db:select(Sql),
    F = fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end,
    parser:convert(Data, rank, F).

%% @doc delete
delete_by_type(Type) ->
    Sql = parser:format(?DELETE_BY_TYPE, [Type]),
    db:delete(Sql).

