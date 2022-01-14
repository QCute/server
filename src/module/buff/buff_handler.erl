-module(buff_handler).
-export([handle/3]).

handle(User, 11801, []) ->
    buff:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
