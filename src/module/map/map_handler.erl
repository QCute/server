-module(map_handler).
-export([handle/3]).

handle(20001, User, []) ->
    map_server:query(User);

handle(20002, User, [X, Y]) ->
    map_server:move(User, X, Y);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
