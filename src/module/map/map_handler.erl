-module(map_handler).
-export([handle/3]).

handle(User, 20001, []) ->
    map_server:query(User);

handle(User, 20006, [X, Y]) ->
    map_server:move(User, X, Y);

handle(User, 20007, [SkillId, TargetList]) ->
    map_server:attack(User, SkillId, TargetList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
