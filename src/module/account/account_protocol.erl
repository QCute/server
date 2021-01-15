-module(account_protocol).
-export([read/2, write/2]).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    AccountNameBinary = db:quote_string(AccountName),
    {ok, [ServerId, AccountNameBinary]};

read(10002, <<RoleNameLength:16, RoleName:RoleNameLength/binary, ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary, Sex:8, Classes:8, ChannelLength:16, Channel:ChannelLength/binary, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    RoleNameBinary = db:quote_string(RoleName),
    AccountNameBinary = db:quote_string(AccountName),
    ChannelBinary = db:quote_string(Channel),
    DeviceIdBinary = db:quote_string(DeviceId),
    MacBinary = db:quote_string(Mac),
    DeviceTypeBinary = db:quote_string(DeviceType),
    {ok, [RoleNameBinary, ServerId, AccountNameBinary, Sex, Classes, ChannelBinary, DeviceIdBinary, MacBinary, DeviceTypeBinary]};

read(10003, <<RoleId:64, RoleNameLength:16, RoleName:RoleNameLength/binary, ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    RoleNameBinary = db:quote_string(RoleName),
    AccountNameBinary = db:quote_string(AccountName),
    {ok, [RoleId, RoleNameBinary, ServerId, AccountNameBinary]};

read(10004, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, List) ->
    {ok, protocol:pack(10001, <<(length(List)):16, <<<<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary>> || {RoleId, RoleName} <- List>>/binary>>)};

write(10002, [Result, RoleId, RoleName]) ->
    {ok, protocol:pack(10002, <<(protocol:text(10002, Result))/binary, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary>>)};

write(10003, Result) ->
    {ok, protocol:pack(10003, <<(protocol:text(10003, Result))/binary>>)};

write(10004, Result) ->
    {ok, protocol:pack(10004, <<(protocol:text(10004, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

