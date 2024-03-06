-module(rank_handler).
-export([handle/3]).
-export([send_level/3]).
-export([send_fight/3]).
-export([send_achievement/3]).
-export([send_wealth/3]).
-export([send_classes/3]).
-include("user.hrl").

handle(User, 19001, []) ->
    rank_server:query(User, 19001);

handle(User, 19002, []) ->
    rank_server:query(User, 19002);

handle(User, 19003, []) ->
    rank_server:query(User, 19003);

handle(User, 19004, []) ->
    rank_server:query(User, 19004);

handle(User, 19005, []) ->
    rank_server:query(User, 19005);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_level(User, 19001, List) ->
    {ok, Binary} = rank_protocol:encode(19001, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fight(User, 19002, List) ->
    {ok, Binary} = rank_protocol:encode(19002, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_achievement(User, 19003, List) ->
    {ok, Binary} = rank_protocol:encode(19003, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_wealth(User, 19004, List) ->
    {ok, Binary} = rank_protocol:encode(19004, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_classes(User, 19005, List) ->
    {ok, Binary} = rank_protocol:encode(19005, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

