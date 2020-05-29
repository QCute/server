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
    {ok, protocol:pack(19101, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} <- List>>/binary>>)};

write(19102, List) ->
    {ok, protocol:pack(19102, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}} <- List>>/binary>>)};

write(19103, List) ->
    {ok, protocol:pack(19103, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}} <- List>>/binary>>)};

write(19104, List) ->
    {ok, protocol:pack(19104, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}} <- List>>/binary>>)};

write(19105, List) ->
    {ok, protocol:pack(19105, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

