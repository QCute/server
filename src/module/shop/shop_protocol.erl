-module(shop_protocol).
-export([read/2, write/2]).
-include("shop.hrl").


read(11301, <<>>) ->
    {ok, []};

read(11302, <<ShopId:32, Number:16>>) ->
    {ok, [ShopId, Number]};

read(Code, Binary) ->
    {error, Code, Binary}.


write(11301, List) ->
    ListBinary = protocol:write_list(fun(#shop{shop_id = ShopId, number = Number}) -> <<ShopId:32, Number:16>> end, List),
    {ok, protocol:pack(11301, <<ListBinary/binary>>)};

write(11302, Result) ->
    {ok, protocol:pack(11302, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


