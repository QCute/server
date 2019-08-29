-module(item_protocol).
-export([read/2, write/2]).
-include("item.hrl").


read(11101, <<>>) ->
    {ok, []};

read(11102, <<>>) ->
    {ok, []};

read(11103, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11101, [List]) ->
    {ok, protocol:pack(11101, <<(length(List)):16, <<<<UniqueId:64, ItemId:32, Type:8, Amount:16, Bind:8>> || #item{unique_id = UniqueId, item_id = ItemId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>)};

write(11102, [List]) ->
    {ok, protocol:pack(11102, <<(length(List)):16, <<<<UniqueId:64, ItemId:32, Type:8, Amount:16, Bind:8>> || #item{unique_id = UniqueId, item_id = ItemId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>)};

write(11103, [List]) ->
    {ok, protocol:pack(11103, <<(length(List)):16, <<<<UniqueId:64, ItemId:32, Type:8, Amount:16, Bind:8>> || #item{unique_id = UniqueId, item_id = ItemId, type = Type, amount = Amount, bind = Bind} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
