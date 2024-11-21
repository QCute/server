-module(cheat_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_cheat/2]).
-include("user.hrl").

handle(User, 60001, {}) ->
    cheat:query(User);

handle(User, 60002, Data) ->
    cheat:cheat(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = cheat_protocol:encode(60001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_cheat(User, Data) ->
    {ok, Binary} = cheat_protocol:encode(60002, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

