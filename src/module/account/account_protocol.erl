-module(account_protocol).
-export([read/2, write/2]).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    {ok, [ServerId, AccountName]};

read(10002, <<ServerId:16, Sex:8, Career:8, ChannelId:16, NameLength:16, Name:NameLength/binary, NickLength:16, Nick:NickLength/binary, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, Sex, Career, ChannelId, Name, Nick, DeviceId, Mac, DeviceType]};

read(10003, <<NameLength:16, Name:NameLength/binary>>) ->
    {ok, [Name]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, [Result]) ->
    {ok, protocol:pack(10001, <<Result:8>>)};

write(10002, [Result]) ->
    {ok, protocol:pack(10002, <<Result:8>>)};

write(10003, [Name]) ->
    {ok, protocol:pack(10003, <<(byte_size(Name)):16, (Name)/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
