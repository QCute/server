-module(item_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


read(11101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11101, [List]) ->
    ListBinary = <<(length(List)):16, <<<<ItemId:32, DataId:32, Type:8, Amount:16, Bind:8>> || #item{item_id = ItemId, data_id = DataId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>,
    {ok, protocol:pack(11101, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
