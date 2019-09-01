-module(role_handler).
-export([handle/3]).

handle(10101, User, []) ->
    role:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
