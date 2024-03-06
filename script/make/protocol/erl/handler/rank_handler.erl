-module(rank_handler).
-export([handle/3]).
-export([send_level/3]).
-export([send_fight/3]).
-export([send_achievement/3]).
-export([send_wealth/3]).
-export([send_classes/3]).
-include("user.hrl").

handle(User, 19001, Data) ->
    rank_server:query(User, 19001, Data);

handle(User, 19002, Data) ->
    rank_server:query(User, 19002, Data);

handle(User, 19003, Data) ->
    rank_server:query(User, 19003, Data);

handle(User, 19004, Data) ->
    rank_server:query(User, 19004, Data);

handle(User, 19005, Data) ->
    rank_server:query(User, 19005, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_level(User, 19001, Data) ->
    {ok, Binary} = rank_protocol:encode(19001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fight(User, 19002, Data) ->
    {ok, Binary} = rank_protocol:encode(19002, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_achievement(User, 19003, Data) ->
    {ok, Binary} = rank_protocol:encode(19003, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_wealth(User, 19004, Data) ->
    {ok, Binary} = rank_protocol:encode(19004, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_classes(User, 19005, Data) ->
    {ok, Binary} = rank_protocol:encode(19005, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

