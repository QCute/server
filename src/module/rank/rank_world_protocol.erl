-module(rank_world_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(19201, <<>>) ->
    {ok, []};

read(19202, <<>>) ->
    {ok, []};

read(19203, <<>>) ->
    {ok, []};

read(19204, <<>>) ->
    {ok, []};

read(19205, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(19201, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> end, List),
    {ok, protocol:pack(19201, <<ListBinary/binary>>)};

write(19202, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> end, List),
    {ok, protocol:pack(19202, <<ListBinary/binary>>)};

write(19203, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> end, List),
    {ok, protocol:pack(19203, <<ListBinary/binary>>)};

write(19204, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> end, List),
    {ok, protocol:pack(19204, <<ListBinary/binary>>)};

write(19205, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> end, List),
    {ok, protocol:pack(19205, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


