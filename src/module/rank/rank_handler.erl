-module(rank_handler).
-export([handle/3]).

handle(19001, _, []) ->
    rank_server:query(19001);

handle(19002, _, []) ->
    rank_server:query(19002);

handle(19003, _, []) ->
    rank_server:query(19003);

handle(19004, _, []) ->
    rank_server:query(19004);

handle(19005, _, []) ->
    rank_server:query(19005);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
