-module(shop_protocol).
-export([read/2, write/2]).
-include("shop.hrl").


read(11301, <<>>) ->
    {ok, []};

read(11302, <<ShopId:32, Amount:16>>) ->
    {ok, [ShopId, Amount]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11301, [List]) ->
    {ok, protocol:pack(11301, <<(length(List)):16, <<<<ShopId:32, Amount:16>> || #shop{shop_id = ShopId, amount = Amount} <- List>>/binary>>)};

write(11302, [Result]) ->
    {ok, protocol:pack(11302, <<Result:8>>)};

write(Code, Content) ->
    {error, Code, Content}.
