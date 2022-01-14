-module(rank_handler).
-export([handle/3]).

handle(_, 19001, []) ->
    rank_server:query(19001);

handle(_, 19002, []) ->
    rank_server:query(19002);

handle(_, 19003, []) ->
    rank_server:query(19003);

handle(_, 19004, []) ->
    rank_server:query(19004);

handle(_, 19005, []) ->
    rank_server:query(19005);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
