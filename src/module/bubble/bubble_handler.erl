-module(bubble_handler).
-export([handle/3]).

handle(User, 12101, []) ->
    bubble:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
