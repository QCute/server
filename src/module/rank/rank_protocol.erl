-module(rank_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


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

read(Code, Binary) ->
    {error, Code, Binary}.



write(19001, List) ->
    {ok, protocol:pack(19001, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} <- List>>/binary>>)};

write(19002, List) ->
    {ok, protocol:pack(19002, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}} <- List>>/binary>>)};

write(19003, List) ->
    {ok, protocol:pack(19003, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}} <- List>>/binary>>)};

write(19004, List) ->
    {ok, protocol:pack(19004, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}} <- List>>/binary>>)};

write(19005, List) ->
    {ok, protocol:pack(19005, <<(length(List)):16, <<<<Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> || #rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

