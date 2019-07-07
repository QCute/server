-module(asset_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("asset.hrl").


read(10201, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10201, [#asset{gold = Gold, silver = Silver, copper = Copper, exp = Exp}]) ->
    AssetBinary = <<Gold:64, Silver:32, Copper:64, Exp:64>>,
    {ok, protocol:pack(10201, <<AssetBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
