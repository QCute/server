-module(rank_world_handler).
-export([handle/3]).

handle(19201, User, []) ->
    rank_server:query_world(User, 19201);

handle(19202, User, []) ->
    rank_server:query_world(User, 19202);

handle(19203, User, []) ->
    rank_server:query_world(User, 19203);

handle(19204, User, []) ->
    rank_server:query_world(User, 19204);

handle(19205, User, []) ->
    rank_server:query_world(User, 19205);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
