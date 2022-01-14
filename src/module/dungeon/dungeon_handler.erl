-module(dungeon_handler).
-export([handle/3]).

handle(User, 17001, []) ->
    dungeon:query(User);

handle(User, 17002, DungeonId) ->
    dungeon:enter(User, DungeonId);

handle(User, 17005, []) ->
    dungeon_map:inspire(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
