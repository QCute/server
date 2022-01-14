-module(fashion_handler).
-export([handle/3]).

handle(User, 12001, []) ->
    fashion:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
