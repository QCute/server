-module(rank_center_handler).
-export([handle/3]).
-export([send_level/3]).
-export([send_fight/3]).
-export([send_achievement/3]).
-export([send_wealth/3]).
-export([send_classes/3]).
-include("user.hrl").

handle(User, 19101, Data) ->
    rank_server:query_center(User, 19101, Data);

handle(User, 19102, Data) ->
    rank_server:query_center(User, 19102, Data);

handle(User, 19103, Data) ->
    rank_server:query_center(User, 19103, Data);

handle(User, 19104, Data) ->
    rank_server:query_center(User, 19104, Data);

handle(User, 19105, Data) ->
    rank_server:query_center(User, 19105, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_level(User, 19101, Data) ->
    {ok, Binary} = rank_center_protocol:encode(19101, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fight(User, 19102, Data) ->
    {ok, Binary} = rank_center_protocol:encode(19102, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_achievement(User, 19103, Data) ->
    {ok, Binary} = rank_center_protocol:encode(19103, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_wealth(User, 19104, Data) ->
    {ok, Binary} = rank_center_protocol:encode(19104, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_classes(User, 19105, Data) ->
    {ok, Binary} = rank_center_protocol:encode(19105, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

