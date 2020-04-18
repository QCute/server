-module(cheat_handler).
-export([handle/3]).

handle(60001, User, []) ->
    cheat:query(User);

handle(60002, User, CommandString) ->
    cheat:cheat(User, CommandString);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
