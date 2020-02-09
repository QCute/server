-module(map_handler).
-export([handle/3]).

handle(20001, User, []) ->
    map_server:query(User);

handle(20006, User, [X, Y]) ->
    map_server:move(User, X, Y);

handle(20007, User, [SkillId, TargetList]) ->
    map_server:attack(User, SkillId, TargetList);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
