-module(protocol_20).
-compile(nowarn_export_all).
-compile(export_all).
-include("rank.hrl").


read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, [List]) ->
    ListBinary = <<(length(List)):16, <<<<Type:16, Key:64, Value:64, Time:32, Rank:64, (byte_size(Name)):16, (Name)/binary>> || #rank{type = Type, key = Key, value = Value, time = Time, rank = Rank, name = Name} <- List>>/binary>>,
    {ok, protocol:pack(20001, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
