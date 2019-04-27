-module(protocol_12).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


read(Code, Binary) ->
    {error, Code, Binary}.



write(12001, [List]) ->
    ListBinary = <<(length(List)):16, <<<<Id:32, DataId:32, Type:8, Amount:16, Bind:8>> || #item{id = Id, data_id = DataId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>,
    {ok, protocol:pack(12001, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
