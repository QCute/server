-module(rank_world_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


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

read(Code, Binary) ->
    {error, Code, Binary}.



write(19201, List) ->
    {ok, protocol:pack(19201, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>> || #rank{type = Type, order = Rank, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} <- List>>/binary>>)};

write(19202, List) ->
    {ok, protocol:pack(19202, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>> || #rank{type = Type, order = Rank, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}} <- List>>/binary>>)};

write(19203, List) ->
    {ok, protocol:pack(19203, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>> || #rank{type = Type, order = Rank, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}} <- List>>/binary>>)};

write(19204, List) ->
    {ok, protocol:pack(19204, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>> || #rank{type = Type, order = Rank, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}} <- List>>/binary>>)};

write(19205, List) ->
    {ok, protocol:pack(19205, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> || #rank{type = Type, order = Rank, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

