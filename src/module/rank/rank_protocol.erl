-module(rank_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


read(19001, <<RankType:8>>) ->
    {ok, RankType};

read(19002, <<RankType:8>>) ->
    {ok, RankType};

read(19003, <<RankType:8>>) ->
    {ok, RankType};

read(19004, <<RankType:8>>) ->
    {ok, RankType};

read(19005, <<RankType:8>>) ->
    {ok, RankType};

read(Code, Binary) ->
    {error, Code, Binary}.



write(19001, List) ->
    {ok, protocol:pack(19001, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name} <- List>>/binary>>)};

write(19002, List) ->
    {ok, protocol:pack(19002, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, Level:16, Classes:8>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name, other = {Level, Classes}} <- List>>/binary>>)};

write(19003, List) ->
    {ok, protocol:pack(19003, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, Level:16, Classes:8, Sex:8>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name, other = {Level, Classes, Sex}} <- List>>/binary>>)};

write(19004, List) ->
    {ok, protocol:pack(19004, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, Level:16, Classes:8, Sex:8, VipLevel:8>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name, other = {Level, Classes, Sex, VipLevel}} <- List>>/binary>>)};

write(19005, List) ->
    {ok, protocol:pack(19005, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name, other = {Level, Classes, Sex, VipLevel, Avatar}} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

