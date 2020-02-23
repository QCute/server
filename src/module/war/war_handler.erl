-module(war_handler).
-export([handle/3]).

handle(18001, User, MonsterId) ->
    boss_server:battle(User, MonsterId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
