-module(protocol_10).
-compile(nowarn_export_all).
-compile(export_all).


read(12345, <<ServerId:16, Sex:8, Career:8, AgentId:16, NameLength:16, Name:NameLength/binary, NickLength:16, Nick:NickLength/binary, DeviceLength:16, Device:DeviceLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, Sex, Career, AgentId, Name, Nick, Device, Mac, DeviceType]};

read(23456, <<ServerId:16, Id:64, NameLength:16, Name:NameLength/binary>>) ->
    {ok, [ServerId, Id, Name]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(34567, [RankList, RankListList]) ->
    RankListBinary = protocol:pack_ets(fun([{Key, Value}]) -> <<Key:8, Value:8>> end, RankList),
    RankListListBinary = protocol:pack_ets(fun(Undefined1) -> <<(length(Undefined1)):16, <<<<Key:8, Value:8>> || {Key, Value} <- Undefined1>>>> end, RankListList),
    {ok, protocol:pack(34567, <<RankListBinary/binary, RankListListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
