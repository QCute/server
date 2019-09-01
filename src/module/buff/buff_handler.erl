-module(buff_handler).
-export([handle/3]).

handle(11801, User, []) ->
    buff:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
