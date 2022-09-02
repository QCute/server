-module(map_protocol).
-export([read/2, write/2]).
-include("map.hrl").
-include("attribute.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(20001, <<>>) ->
    {ok, []};

read(20011, <<>>) ->
    {ok, []};

read(20012, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(20014, <<SkillId:32, Binary/binary>>) ->
    {TargetList, _} = protocol:read_list(fun(TargetListBinary) -> <<TargetId:64, TargetListInnerRest/binary>> = TargetListBinary, {TargetId, TargetListInnerRest} end, Binary),
    {ok, [SkillId, TargetList]};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(20001, []) ->
    {ok, protocol:pack(20001, <<>>)};

write(20011, List) ->
    ListBinary = protocol:write_list(fun(#fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp, health = Health}, skill = Skill, buff = Buff, x = X, y = Y}) -> SkillBinary = protocol:write_list(fun(#battle_skill{skill_id = SkillId, time = Time, number = Number}) -> <<SkillId:32, Time:32, Number:32>> end, Skill), BuffBinary = protocol:write_list(fun(#battle_buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap}) -> <<BuffId:32, ExpireTime:32, Overlap:32>> end, Buff), <<Id:64, Type:8, Fc:64, Hp:64, Health:64, SkillBinary/binary, BuffBinary/binary, X:16, Y:16>> end, List),
    {ok, protocol:pack(20011, <<ListBinary/binary>>)};

write(20012, #fighter{id = Id, x = X, y = Y}) ->
    {ok, protocol:pack(20012, <<Id:64, X:16, Y:16>>)};

write(20013, #fighter{id = Id}) ->
    {ok, protocol:pack(20013, <<Id:64>>)};

write(20014, [FighterId, PerformSkillId, List]) ->
    ListBinary = protocol:write_list(fun(#fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp, health = Health}, skill = Skill, buff = Buff, x = X, y = Y}) -> SkillBinary = protocol:write_list(fun(#battle_skill{skill_id = SkillId, time = Time, number = Number}) -> <<SkillId:32, Time:32, Number:32>> end, Skill), BuffBinary = protocol:write_list(fun(#battle_buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap}) -> <<BuffId:32, ExpireTime:32, Overlap:32>> end, Buff), <<Id:64, Type:8, Fc:64, Hp:64, Health:64, SkillBinary/binary, BuffBinary/binary, X:16, Y:16>> end, List),
    {ok, protocol:pack(20014, <<FighterId:64, PerformSkillId:32, ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


