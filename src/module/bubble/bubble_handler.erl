-module(bubble_handler).
-export([handle/3]).

handle(12101, User, []) ->
    bubble:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
