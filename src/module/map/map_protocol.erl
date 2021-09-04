-module(map_protocol).
-export([read/2, write/2]).
-include("map.hrl").
-include("attribute.hrl").


read(20001, <<>>) ->
    {ok, []};

read(20006, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(20007, <<SkillId:32, Binary/binary>>) ->
    {TargetList, _} = protocol:read_list(fun(TargetListBinary) -> <<TargetId:64, TargetListInnerRest/binary>> = TargetListBinary, {TargetId, TargetListInnerRest} end, Binary),
    {ok, [SkillId, TargetList]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, []) ->
    {ok, protocol:pack(20001, <<>>)};

write(20002, #fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp}, x = X, y = Y}) ->
    {ok, protocol:pack(20002, <<Id:64, Type:8, Fc:64, Hp:64, X:16, Y:16>>)};

write(20003, List) ->
    ListBinary = protocol:write_list(fun(#fighter{id = Id, type = Type, attribute = #attribute{hp = Hp}, x = X, y = Y}) -> <<Id:64, Type:8, Hp:64, X:16, Y:16>> end, List),
    {ok, protocol:pack(20003, <<ListBinary/binary>>)};

write(20004, List) ->
    ListBinary = protocol:write_list(fun(#fighter{id = Id, type = Type, x = X, y = Y}) -> <<Id:64, Type:8, X:16, Y:16>> end, List),
    {ok, protocol:pack(20004, <<ListBinary/binary>>)};

write(20005, List) ->
    ListBinary = protocol:write_list(fun(#fighter{id = Id}) -> <<Id:64>> end, List),
    {ok, protocol:pack(20005, <<ListBinary/binary>>)};

write(20006, []) ->
    {ok, protocol:pack(20006, <<>>)};

write(20007, [Id, SkillId, TargetList]) ->
    TargetListBinary = protocol:write_list(fun(#fighter{id = TargetId}) -> <<TargetId:64>> end, TargetList),
    {ok, protocol:pack(20007, <<Id:64, SkillId:32, TargetListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

