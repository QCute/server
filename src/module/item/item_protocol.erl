-module(item_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


read(11101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11101, [List]) ->
    {ok, protocol:pack(11101, <<(length(List)):16, <<<<UniqueId:64, ItemId:32, Type:8, Amount:16, Bind:8>> || #item{unique_id = UniqueId, item_id = ItemId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
