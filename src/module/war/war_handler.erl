-module(war_handler).
-export([handle/3]).

handle(User, 18001, MonsterId) ->
    boss_server:battle(User, MonsterId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
