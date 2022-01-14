-module(item_protocol).
-export([read/2, write/2]).
-include("item.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11101, <<>>) ->
    {ok, []};

read(11102, <<>>) ->
    {ok, []};

read(11103, <<>>) ->
    {ok, []};

read(11106, <<ItemNo:64, Number:16, Type:8>>) ->
    {ok, [ItemNo, Number, Type]};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11101, List) ->
    ListBinary = protocol:write_list(fun(#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number}) -> <<ItemNo:64, ItemId:32, Type:8, Number:16>> end, List),
    {ok, protocol:pack(11101, <<ListBinary/binary>>)};

write(11102, List) ->
    ListBinary = protocol:write_list(fun(#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number}) -> <<ItemNo:64, ItemId:32, Type:8, Number:16>> end, List),
    {ok, protocol:pack(11102, <<ListBinary/binary>>)};

write(11103, List) ->
    ListBinary = protocol:write_list(fun(#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number}) -> <<ItemNo:64, ItemId:32, Type:8, Number:16>> end, List),
    {ok, protocol:pack(11103, <<ListBinary/binary>>)};

write(11104, List) ->
    ListBinary = protocol:write_list(fun(#item{item_no = ItemNo, type = Type}) -> <<ItemNo:64, Type:8>> end, List),
    {ok, protocol:pack(11104, <<ListBinary/binary>>)};

write(11106, Result) ->
    {ok, protocol:pack(11106, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


