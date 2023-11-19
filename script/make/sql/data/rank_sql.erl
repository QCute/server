-module(rank_sql).
-export([save/1]).
-export([select_by_type/1]).
-export([delete_by_type/1]).
-include("rank.hrl").

%% @doc insert into rank
-spec save(RankList :: [#rank{}] | ets:tab()) -> NewRankList :: [#rank{}].
save(RankList) ->
    db:save_into(<<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:)">>, <<"ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `order` = VALUES(`order`), `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `server_id` = VALUES(`server_id`), `digest` = VALUES(`digest`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)">>, RankList, #rank.flag).

%% @doc select from rank
-spec select_by_type(Type :: non_neg_integer()) -> Rows :: [#rank{}].
select_by_type(Type) ->
    Data = db:select(<<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, `flag` FROM `rank` WHERE `type` = ?">>, [Type]),
    parser:convert(Data, rank, fun(Rank = #rank{digest = Digest, extra = Extra, other = Other}) -> Rank#rank{digest = parser:to_term(Digest), extra = parser:to_term(Extra), other = parser:to_term(Other)} end).

%% @doc delete row from rank
-spec delete_by_type(Type :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_type(Type) ->
    db:delete(<<"DELETE FROM `rank` WHERE `type` = ?">>, [Type]).
