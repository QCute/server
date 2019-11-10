-module(account_protocol).
-export([read/2, write/2]).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountLength:16, Account:AccountLength/binary>>) ->
    {ok, [ServerId, Account]};

read(10002, <<AccountLength:16, Account:AccountLength/binary, RoleNameLength:16, RoleName:RoleNameLength/binary, ServerId:16, Sex:8, Classes:8, ChannelId:16, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [Account, RoleName, ServerId, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, [Result]) ->
    {ok, protocol:pack(10001, <<Result:8>>)};

write(10002, [Result]) ->
    {ok, protocol:pack(10002, <<Result:8>>)};

write(Code, Content) ->
    {error, Code, Content}.
