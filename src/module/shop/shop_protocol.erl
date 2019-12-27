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
    {ok, protocol:pack(11301, <<(length(List)):16, <<<<ShopId:32, Number:16>> || #shop{shop_id = ShopId, number = Number} <- List>>/binary>>)};

write(11302, Result) ->
    {ok, protocol:pack(11302, <<(text(11302, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(11302, asset_not_enough) ->
    <<12:16, "资产不足"/utf8>>;
text(11302, buy_max) ->
    <<21:16, "已达到购买上限"/utf8>>;
text(11302, configure_not_found) ->
    <<12:16, "配置错误"/utf8>>;
text(11302, level_not_enough) ->
    <<15:16, "等级不满足"/utf8>>;
text(11302, number_invalid) ->
    <<18:16, "购买数量错误"/utf8>>;
text(11302, vip_level_not_enough) ->
    <<18:16, "Vip等级不满足"/utf8>>;
text(_, 0) ->
    <<0:16>>;
text(_, ok) ->
    <<0:16>>;
text(_, Reason) ->
    <<(protocol:write_bit_string(type:to_binary(Reason)))/binary>>.

