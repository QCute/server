-module(rank_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(19001, <<>>) ->
    {ok, []};

read(19002, <<>>) ->
    {ok, []};

read(19003, <<>>) ->
    {ok, []};

read(19004, <<>>) ->
    {ok, []};

read(19005, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(19001, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> end, List),
    {ok, protocol:pack(19001, <<ListBinary/binary>>)};

write(19002, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> end, List),
    {ok, protocol:pack(19002, <<ListBinary/binary>>)};

write(19003, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> end, List),
    {ok, protocol:pack(19003, <<ListBinary/binary>>)};

write(19004, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> end, List),
    {ok, protocol:pack(19004, <<ListBinary/binary>>)};

write(19005, List) ->
    ListBinary = protocol:write_list(fun(#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}}) -> <<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> end, List),
    {ok, protocol:pack(19005, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


