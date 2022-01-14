-module(title_handler).
-export([handle/3]).

handle(User, 11901, []) ->
    title:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
