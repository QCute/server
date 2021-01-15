-module(map_protocol).
-export([read/2, write/2]).
-include("map.hrl").
-include("attribute.hrl").


read(20001, <<>>) ->
    {ok, []};

read(20006, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(20007, <<SkillId:32, TargetListLength:16, TargetListBinary:TargetListLength/binary-unit:64>>) ->
    TargetList = [TargetId || <<TargetId:64>> <= TargetListBinary],
    {ok, [SkillId, TargetList]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, []) ->
    {ok, protocol:pack(20001, <<>>)};

write(20002, #fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp}, x = X, y = Y}) ->
    {ok, protocol:pack(20002, <<Id:64, Type:8, Fc:64, Hp:64, X:16, Y:16>>)};

write(20003, List) ->
    {ok, protocol:pack(20003, <<(length(List)):16, <<<<Id:64, Type:8, Hp:64, X:16, Y:16>> || #fighter{id = Id, type = Type, attribute = #attribute{hp = Hp}, x = X, y = Y} <- List>>/binary>>)};

write(20004, List) ->
    {ok, protocol:pack(20004, <<(length(List)):16, <<<<Id:64, Type:8, X:16, Y:16>> || #fighter{id = Id, type = Type, x = X, y = Y} <- List>>/binary>>)};

write(20005, List) ->
    {ok, protocol:pack(20005, <<(length(List)):16, <<<<Id:64>> || #fighter{id = Id} <- List>>/binary>>)};

write(20006, []) ->
    {ok, protocol:pack(20006, <<>>)};

write(20007, [Id, SkillId, TargetList]) ->
    {ok, protocol:pack(20007, <<Id:64, SkillId:32, (length(TargetList)):16, <<<<TargetId:64>> || #fighter{id = TargetId} <- TargetList>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

