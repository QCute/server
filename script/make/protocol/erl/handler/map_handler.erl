-module(map_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_fighter/2]).
-export([send_fighter_move/2]).
-export([send_attack/2]).
-include("user.hrl").

handle(User, 20001, Data) ->
    map_server:query(User, Data);

handle(User, 20012, Data) ->
    map_server:move(User, Data);

handle(User, 20014, Data) ->
    map_server:attack(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = map_protocol:encode(20001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fighter(User, Data) ->
    {ok, Binary} = map_protocol:encode(20011, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fighter_move(User, Fighter) ->
    {ok, Binary} = map_protocol:encode(20012, Fighter),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_attack(User, Data) ->
    {ok, Binary} = map_protocol:encode(20014, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

