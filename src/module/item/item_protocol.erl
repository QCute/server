-module(item_protocol).
-export([read/2, write/2]).
-include("item.hrl").


read(11101, <<>>) ->
    {ok, []};

read(11102, <<>>) ->
    {ok, []};

read(11103, <<>>) ->
    {ok, []};

read(11106, <<ItemNo:64, Number:16, Type:8>>) ->
    {ok, [ItemNo, Number, Type]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11101, List) ->
    {ok, protocol:pack(11101, <<(length(List)):16, <<<<ItemNo:64, ItemId:32, Type:8, Number:16>> || #item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} <- List>>/binary>>)};

write(11102, List) ->
    {ok, protocol:pack(11102, <<(length(List)):16, <<<<ItemNo:64, ItemId:32, Type:8, Number:16>> || #item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} <- List>>/binary>>)};

write(11103, List) ->
    {ok, protocol:pack(11103, <<(length(List)):16, <<<<ItemNo:64, ItemId:32, Type:8, Number:16>> || #item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} <- List>>/binary>>)};

write(11104, List) ->
    {ok, protocol:pack(11104, <<(length(List)):16, <<<<ItemNo:64, Type:8>> || #item{item_no = ItemNo, type = Type} <- List>>/binary>>)};

write(11106, Result) ->
    {ok, protocol:pack(11106, <<(protocol:text(11106, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

