-module(cheat_handler).
-export([handle/3]).

handle(60001, User, []) ->
    cheat:query(User);

handle(60002, User, Command) ->
    cheat:cheat(User, Command);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
