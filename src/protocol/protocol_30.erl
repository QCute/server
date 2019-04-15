-module(protocol_30).
-compile(nowarn_export_all).
-compile(export_all).
-include("rank.hrl").


read(30001, <<RankType:32>>) ->
    {ok, [RankType]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(30001, [List]) ->
    ListBinary = <<(length(List)):16, <<<<Type:32, Key:64, Value:64, Time:32, Rank:64, (byte_size(Name)):16, (Name)/binary>> || #rank{type = Type, key = Key, value = Value, time = Time, rank = Rank, name = Name} <- List>>/binary>>,
    {ok, protocol:pack(30001, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
