-module(cheat_handler).
-export([handle/3]).

handle(User, 60001, []) ->
    cheat:query(User);

handle(User, 60002, Command) ->
    cheat:cheat(User, Command);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
