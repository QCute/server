-module(dungeon_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_enter/2]).
-export([send_inspire/2]).
-include("user.hrl").

handle(User, 17001, {}) ->
    dungeon:query(User);

handle(User, 17002, Data) ->
    dungeon:enter(User, Data);

handle(User, 17005, {}) ->
    dungeon_map:inspire(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = dungeon_protocol:encode(17001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_enter(User, Data) ->
    {ok, Binary} = dungeon_protocol:encode(17002, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_inspire(User, Data) ->
    {ok, Binary} = dungeon_protocol:encode(17005, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

