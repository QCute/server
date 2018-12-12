-module(protocol_10).
-compile(nowarn_export_all).
-compile(export_all).


read(12345, <<ServerId:16, Sex:16, Career:8, AgentId:16, NameLength:16, Name:NameLength/binary, NickLength:16, Nick:NickLength/binary, DeviceLength:16, Device:DeviceLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, Sex, Career, AgentId, binary_to_list(Name), binary_to_list(Nick), binary_to_list(Device), binary_to_list(Mac), binary_to_list(DeviceType)]};

read(23456, <<ServerId:16, Id:64, NameLength:16, Name:NameLength/binary>>) ->
    {ok, [ServerId, Id, binary_to_list(Name)]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(Code, Content) ->
    {error, Code, Content}.
