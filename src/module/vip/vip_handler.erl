-module(vip_handler).
-export([handle/3]).

handle(10301, User, []) ->
    vip:push(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
