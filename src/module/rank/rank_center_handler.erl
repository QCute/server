-module(rank_center_handler).
-export([handle/3]).

handle(19101, User, []) ->
    rank_server:query_center(User, 19101);

handle(19102, User, []) ->
    rank_server:query_center(User, 19102);

handle(19103, User, []) ->
    rank_server:query_center(User, 19103);

handle(19104, User, []) ->
    rank_server:query_center(User, 19104);

handle(19105, User, []) ->
    rank_server:query_center(User, 19105);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
