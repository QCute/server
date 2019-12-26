-module(rank_handler).
-export([handle/3]).

handle(19001, _, RankType) ->
    rank_server:query(RankType);

handle(19002, _, RankType) ->
    rank_server:query(RankType);

handle(19003, _, RankType) ->
    rank_server:query(RankType);

handle(19004, _, RankType) ->
    rank_server:query(RankType);

handle(19005, _, RankType) ->
    rank_server:query(RankType);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
