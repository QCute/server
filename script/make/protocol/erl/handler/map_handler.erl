-module(map_handler).
-export([handle/3]).
-export([send_map_server_query/1]).
-export([send_map_server_fighter_list/2]).
-export([send_map_server_move/2]).
-export([send_map_server_attack/4]).
-include("user.hrl").

handle(User, 20001, []) ->
    map_server:query(User);

handle(User, 20011, []) ->
    map_server:fighter_list(User);

handle(User, 20012, [X, Y]) ->
    map_server:move(User, X, Y);

handle(User, 20014, [SkillId, TargetList]) ->
    map_server:attack(User, SkillId, TargetList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_map_server_query(User) ->
    {ok, Binary} = map_protocol:encode(20001, []),
    User#user{buffer = [Binary | User#user.buffer]}.

send_map_server_fighter_list(User, List) ->
    {ok, Binary} = map_protocol:encode(20011, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_map_server_move(User, Fighter) ->
    {ok, Binary} = map_protocol:encode(20012, Fighter),
    User#user{buffer = [Binary | User#user.buffer]}.

send_map_server_attack(User, FighterId, PerformSkillId, List) ->
    {ok, Binary} = map_protocol:encode(20014, [FighterId, PerformSkillId, List]),
    User#user{buffer = [Binary | User#user.buffer]}.

