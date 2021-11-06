-module(rank_center_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


read(19101, <<>>) ->
    {ok, []};

read(19102, <<>>) ->
    {ok, []};

read(19103, <<>>) ->
    {ok, []};

read(19104, <<>>) ->
    {ok, []};

read(19105, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.


write(19101, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> end, List),
    {ok, protocol:pack(19101, <<ListBinary/binary>>)};

write(19102, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> end, List),
    {ok, protocol:pack(19102, <<ListBinary/binary>>)};

write(19103, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> end, List),
    {ok, protocol:pack(19103, <<ListBinary/binary>>)};

write(19104, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> end, List),
    {ok, protocol:pack(19104, <<ListBinary/binary>>)};

write(19105, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> end, List),
    {ok, protocol:pack(19105, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


