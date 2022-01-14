-module(account_protocol).
-export([read/2, write/2]).


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    {ok, [ServerId, AccountName]};

read(10002, <<RoleNameLength:16, RoleName:RoleNameLength/binary, ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary, Sex:8, Classes:8, ChannelLength:16, Channel:ChannelLength/binary, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]};

read(10003, <<RoleId:64, RoleNameLength:16, RoleName:RoleNameLength/binary, ServerId:16, AccountNameLength:16, AccountName:AccountNameLength/binary>>) ->
    {ok, [RoleId, RoleName, ServerId, AccountName]};

read(10004, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(10000, Result) ->
    {ok, protocol:pack(10000, <<(protocol:text(Result))/binary>>)};

write(10001, [Result, List]) ->
    ListBinary = protocol:write_list(fun({RoleId, RoleName}) -> <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary>> end, List),
    {ok, protocol:pack(10001, <<(protocol:text(Result))/binary, ListBinary/binary>>)};

write(10002, [Result, RoleId, RoleName]) ->
    {ok, protocol:pack(10002, <<(protocol:text(Result))/binary, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary>>)};

write(10003, Result) ->
    {ok, protocol:pack(10003, <<(protocol:text(Result))/binary>>)};

write(10004, Result) ->
    {ok, protocol:pack(10004, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


