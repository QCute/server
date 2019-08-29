-module(asset_protocol).
-export([read/2, write/2]).
-include("asset.hrl").


read(10201, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10201, [#asset{gold = Gold, silver = Silver, copper = Copper, exp = Exp}]) ->
    {ok, protocol:pack(10201, <<Gold:64, Silver:32, Copper:64, Exp:64>>)};

write(Code, Content) ->
    {error, Code, Content}.
