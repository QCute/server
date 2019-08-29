-module(rank_protocol).
-export([read/2, write/2]).
-include("rank.hrl").


read(19001, <<Type:8>>) ->
    {ok, [Type]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(19001, [List]) ->
    {ok, protocol:pack(19001, <<(length(List)):16, <<<<Type:16, Rank:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary>> || #rank{type = Type, rank = Rank, key = Key, value = Value, time = Time, name = Name} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
