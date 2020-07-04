-module(role_handler).
-export([handle/3]).

handle(10101, User, []) ->
    role:query(User);

handle(10102, User, []) ->
    asset:query(User);

handle(10103, User, []) ->
    vip:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
