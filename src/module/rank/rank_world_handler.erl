-module(rank_world_handler).
-export([handle/3]).

handle(User, 19201, []) ->
    rank_server:query_world(User, 19201);

handle(User, 19202, []) ->
    rank_server:query_world(User, 19202);

handle(User, 19203, []) ->
    rank_server:query_world(User, 19203);

handle(User, 19204, []) ->
    rank_server:query_world(User, 19204);

handle(User, 19205, []) ->
    rank_server:query_world(User, 19205);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
