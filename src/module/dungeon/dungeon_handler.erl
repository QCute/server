-module(dungeon_handler).
-export([handle/3]).

handle(17001, User, []) ->
    dungeon:query(User);

handle(17002, User, DungeonId) ->
    dungeon:enter(User, DungeonId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
