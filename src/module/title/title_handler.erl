-module(title_handler).
-export([handle/3]).

handle(11901, User, []) ->
    title:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
