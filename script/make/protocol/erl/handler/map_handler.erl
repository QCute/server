-module(map_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_fighter/2]).
-export([send_fighter_move/2]).
-export([send_attack/4]).
-include("user.hrl").

handle(User, 20001, {}) ->
    map_server:query(User);

handle(User, 20012, {X, Y}) ->
    map_server:move(User, X, Y);

handle(User, 20014, {SkillId, TargetList}) ->
    map_server:attack(User, SkillId, TargetList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = map_protocol:encode(20001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fighter(User, Data) ->
    {ok, Binary} = map_protocol:encode(20011, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_fighter_move(User, Data) ->
    {ok, Binary} = map_protocol:encode(20012, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_attack(User, FighterId, PerformSkillId, FighterList) ->
    {ok, Binary} = map_protocol:encode(20014, {FighterId, PerformSkillId, FighterList}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

