-module(cheat_handler).
-export([handle/3]).

handle(60000, User, CommandString) ->
    cheat:cheat(User, CommandString);

handle(60001, User, []) ->
    cheat:query(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
