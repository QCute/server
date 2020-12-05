-module(account_protocol).
-export([read/2, write/2]).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    {ok, [ServerId, AccountName]};

read(10002, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary, RoleNameLength:16, RoleName:RoleNameLength/binary, Sex:8, Classes:8, ChannelLength:16, Channel:ChannelLength/binary, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, AccountName, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]};

read(10003, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    {ok, [ServerId, AccountName]};

read(10004, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, List) ->
    {ok, protocol:pack(10001, <<(length(List)):16, <<<<(byte_size(RoleName)):16, (RoleName)/binary>> || RoleName <- List>>/binary>>)};

write(10002, Result) ->
    {ok, protocol:pack(10002, <<(protocol:text(10002, Result))/binary>>)};

write(10003, Result) ->
    {ok, protocol:pack(10003, <<(protocol:text(10003, Result))/binary>>)};

write(10004, Result) ->
    {ok, protocol:pack(10004, <<(protocol:text(10004, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

