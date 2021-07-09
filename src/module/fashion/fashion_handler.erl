-module(fashion_handler).
-export([handle/3]).

handle(12001, User, []) ->
    fashion:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
