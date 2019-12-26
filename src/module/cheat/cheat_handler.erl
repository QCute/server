-module(cheat_handler).
-export([handle/3]).

handle(60000, User, Command) ->
    cheat:cheat(User, Command);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
