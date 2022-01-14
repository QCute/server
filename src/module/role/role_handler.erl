-module(role_handler).
-export([handle/3]).

handle(User, 10101, []) ->
    role:query(User);

handle(User, 10102, []) ->
    asset:query(User);

handle(User, 10103, []) ->
    vip:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
