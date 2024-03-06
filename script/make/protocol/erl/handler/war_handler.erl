-module(war_handler).
-export([handle/3]).
-export([send_boss_server_battle/2]).
-include("user.hrl").

handle(User, 18001, MonsterId) ->
    boss_server:battle(User, MonsterId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_boss_server_battle(User, Result) ->
    {ok, Binary} = war_protocol:encode(18001, Result),
    User#user{buffer = [Binary | User#user.buffer]}.

