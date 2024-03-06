-module(war_handler).
-export([handle/3]).
-export([send_battle/2]).
-include("user.hrl").

handle(User, 18001, Data) ->
    boss_server:battle(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_battle(User, Data) ->
    {ok, Binary} = war_protocol:encode(18001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

