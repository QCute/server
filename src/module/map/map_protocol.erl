-module(map_protocol).
-export([read/2, write/2]).
-include("map.hrl").


read(20001, <<>>) ->
    {ok, []};

read(20002, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, [MapId, X, Y]) ->
    {ok, protocol:pack(20001, <<MapId:32, X:16, Y:16>>)};

write(20002, [X, Y]) ->
    {ok, protocol:pack(20002, <<X:16, Y:16>>)};

write(Code, Content) ->
    {error, Code, Content}.
