-module(protocol_11).
-compile(nowarn_export_all).
-compile(export_all).
-include("player.hrl").
-include("assets.hrl").


read(Code, Binary) ->
    {error, Code, Binary}.



write(11001, [#player{id = Id, sex = Sex, level = Level, classes = Classes, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}]) ->
    PlayerBinary = <<Id:64, Sex:8, Level:64, Classes:8, ItemSize:16, BagSize:16, StoreSize:16>>,
    {ok, protocol:pack(11001, <<PlayerBinary/binary>>)};

write(11002, [#assets{gold = Gold, silver = Silver, copper = Copper, exp = Exp}]) ->
    AssetsBinary = <<Gold:64, Silver:32, Copper:64, Exp:64>>,
    {ok, protocol:pack(11002, <<AssetsBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
