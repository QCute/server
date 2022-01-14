-module(shop_protocol).
-export([read/2, write/2]).
-include("shop.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11301, <<>>) ->
    {ok, []};

read(11302, <<ShopId:32, Number:16>>) ->
    {ok, [ShopId, Number]};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11301, List) ->
    ListBinary = protocol:write_list(fun(#shop{shop_id = ShopId, number = Number}) -> <<ShopId:32, Number:16>> end, List),
    {ok, protocol:pack(11301, <<ListBinary/binary>>)};

write(11302, Result) ->
    {ok, protocol:pack(11302, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


