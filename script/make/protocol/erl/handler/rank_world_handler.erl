-module(rank_world_handler).
-export([handle/3]).
-export([send_rank_server_level/3]).
-export([send_rank_server_fight/3]).
-export([send_rank_server_achievement/3]).
-export([send_rank_server_wealth/3]).
-export([send_rank_server_classes/3]).
-include("user.hrl").

handle(User, 19201, []) ->
    rank_server:query_world(User, 19201);

handle(User, 19202, []) ->
    rank_server:query_world(User, 19202);

handle(User, 19203, []) ->
    rank_server:query_world(User, 19203);

handle(User, 19204, []) ->
    rank_server:query_world(User, 19204);

handle(User, 19205, []) ->
    rank_server:query_world(User, 19205);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_rank_server_level(User, 19201, List) ->
    {ok, Binary} = rank_world_protocol:encode(19201, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_rank_server_fight(User, 19202, List) ->
    {ok, Binary} = rank_world_protocol:encode(19202, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_rank_server_achievement(User, 19203, List) ->
    {ok, Binary} = rank_world_protocol:encode(19203, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_rank_server_wealth(User, 19204, List) ->
    {ok, Binary} = rank_world_protocol:encode(19204, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_rank_server_classes(User, 19205, List) ->
    {ok, Binary} = rank_world_protocol:encode(19205, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

