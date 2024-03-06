-module(rank_sql).
-export([save/1]).
-export([select_by_type/1]).
-export([delete_by_type/1]).
-include("rank.hrl").

%% @doc insert into rank
-spec save(RankList :: [#rank{}] | ets:tab()) -> NewRankList :: [#rank{}].
save(RankList) ->
    db:save(<<"INSERT INTO `rank` (`type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, RankList, fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, digest = Digest, extra = Extra, other = Other}) -> [Type, Order, Key, Value, Time, Name, ServerId, Digest, Extra, Other] end, #rank.flag).

%% @doc select from rank
-spec select_by_type(Type :: non_neg_integer()) -> Rows :: [#rank{}].
select_by_type(Type) ->
    Data = db:select(<<"SELECT `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` FROM `rank` WHERE `type` = ?">>, [Type]),
    parser:convert(Data, rank).

%% @doc delete row from rank
-spec delete_by_type(Type :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_type(Type) ->
    db:delete(<<"DELETE FROM `rank` WHERE `type` = ?">>, [Type]).
