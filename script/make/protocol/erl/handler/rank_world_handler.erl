-module(rank_world_handler).
-export([handle/3]).
-export([send_level/3]).
-export([send_fight/3]).
-export([send_achievement/3]).
-export([send_wealth/3]).
-export([send_classes/3]).
-include("user.hrl").

handle(User, 19201, {}) ->
    rank_server:query_world(User, 19201);

handle(User, 19202, {}) ->
    rank_server:query_world(User, 19202);

handle(User, 19203, {}) ->
    rank_server:query_world(User, 19203);

handle(User, 19204, {}) ->
    rank_server:query_world(User, 19204);

handle(User, 19205, {}) ->
    rank_server:query_world(User, 19205);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_level(User, 19201, Data) ->
    {ok, Binary} = rank_world_protocol:encode(19201, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fight(User, 19202, Data) ->
    {ok, Binary} = rank_world_protocol:encode(19202, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_achievement(User, 19203, Data) ->
    {ok, Binary} = rank_world_protocol:encode(19203, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_wealth(User, 19204, Data) ->
    {ok, Binary} = rank_world_protocol:encode(19204, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_classes(User, 19205, Data) ->
    {ok, Binary} = rank_world_protocol:encode(19205, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

