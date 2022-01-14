-module(rank_center_handler).
-export([handle/3]).

handle(User, 19101, []) ->
    rank_server:query_center(User, 19101);

handle(User, 19102, []) ->
    rank_server:query_center(User, 19102);

handle(User, 19103, []) ->
    rank_server:query_center(User, 19103);

handle(User, 19104, []) ->
    rank_server:query_center(User, 19104);

handle(User, 19105, []) ->
    rank_server:query_center(User, 19105);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
