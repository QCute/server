-module(secret_handler).
-export([handle/3]).

handle(60000, User, [Command]) ->
    secret:cheat(User, Command);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
