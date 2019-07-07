-module(shop_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("shop.hrl").


read(Code, Binary) ->
    {error, Code, Binary}.



write(11301, [List]) ->
    ListBinary = <<(length(List)):16, <<<<ShopId:32, Amount:16>> || #shop{shop_id = ShopId, amount = Amount} <- List>>/binary>>,
    {ok, protocol:pack(11301, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
