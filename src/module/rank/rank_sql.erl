-module(rank_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_type/1]).
-export([delete_by_type/1]).
-include("rank.hrl").

-define(INSERT_RANK, <<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES (~i~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w'~i)">>).
-define(SELECT_RANK, <<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, 1 AS `flag` FROM `rank` WHERE `type` = ~w AND `order` = ~w">>).
-define(UPDATE_RANK, {<<"UPDATE `rank` SET ~i~i~i`key` = ~w, `value` = ~w, `time` = ~w, `name` = '~s', `server_id` = ~w, `digest` = '~w', `extra` = '~w', `other` = '~w'~i ">>, <<"WHERE `type` = ~w AND `order` = ~w">>}).
-define(DELETE_RANK, <<"DELETE FROM `rank` WHERE `type` = ~w AND `order` = ~w">>).
-define(INSERT_UPDATE_RANK, {<<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, '~s', ~w, '~w', '~w', '~w'~i)">>, <<" ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `server_id` = VALUES(`server_id`), `digest` = VALUES(`digest`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)">>}).
-define(SELECT_BY_TYPE, <<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, 1 AS `flag` FROM `rank` WHERE `type` = ~w">>).
-define(SELECT_JOIN_BY_TYPE, <<"SELECT `rank`.`type`, `rank`.`order`, `rank`.`key`, `rank`.`value`, `rank`.`time`, `rank`.`name`, `rank`.`server_id`, `rank`.`digest`, `rank`.`extra`, `rank`.`other`, IFNULL(`rank`.`flag`, 1) AS `flag` FROM `rank` WHERE `rank`.`type` = ~w">>).
-define(DELETE_BY_TYPE, <<"DELETE FROM `rank` WHERE `type` = ~w">>).

%% @doc insert
-spec insert(Rank :: #rank{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Rank) ->
    Sql = parser:format(?INSERT_RANK, Rank),
    db:insert(Sql).

%% @doc select
-spec select(Type :: integer(), Order :: integer()) -> RankList :: [#rank{}].
select(Type, Order) ->
    Sql = parser:format(?SELECT_RANK, [Type, Order]),
    Data = db:select(Sql),
    F = fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end,
    parser:convert(Data, rank, F).

%% @doc update
-spec update(Rank :: #rank{}) -> AffectedRows :: non_neg_integer().
update(Rank) ->
    Sql = <<(parser:format(element(1, ?UPDATE_RANK), Rank))/binary, (parser:format(element(2, ?UPDATE_RANK), [Rank#rank.type, Rank#rank.order]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(Type :: integer(), Order :: integer()) -> AffectedRows :: non_neg_integer().
delete(Type, Order) ->
    Sql = parser:format(?DELETE_RANK, [Type, Order]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(RankList :: [#rank{}] | ets:tab()) -> NewRankList :: [#rank{}].
insert_update(RankList) ->
    {Sql, NewRankList} = parser:collect_into(RankList, ?INSERT_UPDATE_RANK, #rank.flag),
    db:insert(Sql),
    NewRankList.

%% @doc select
-spec select_by_type(Type :: integer()) -> RankList :: [#rank{}].
select_by_type(Type) ->
    Sql = parser:format(?SELECT_BY_TYPE, [Type]),
    Data = db:select(Sql),
    F = fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end,
    parser:convert(Data, rank, F).

%% @doc delete
-spec delete_by_type(Type :: integer()) -> AffectedRows :: non_neg_integer().
delete_by_type(Type) ->
    Sql = parser:format(?DELETE_BY_TYPE, [Type]),
    db:delete(Sql).

