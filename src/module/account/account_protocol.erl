-module(account_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, NameLength:16, Name:NameLength/binary>>) ->
    {ok, [ServerId, Name]};

read(10002, <<ServerId:16, Sex:8, Career:8, AgentId:16, NameLength:16, Name:NameLength/binary, NickLength:16, Nick:NickLength/binary, DeviceLength:16, Device:DeviceLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, Sex, Career, AgentId, Name, Nick, Device, Mac, DeviceType]};

read(10003, <<ServerId:16, Id:64, NameLength:16, Name:NameLength/binary>>) ->
    {ok, [ServerId, Id, Name]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, [Result]) ->
    ResultBinary = <<Result:8>>,
    {ok, protocol:pack(10001, <<ResultBinary/binary>>)};

write(10002, []) ->
    {ok, protocol:pack(10002, <<>>)};

write(10003, []) ->
    {ok, protocol:pack(10003, <<>>)};

write(Code, Content) ->
    {error, Code, Content}.
