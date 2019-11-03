-module(map_protocol).
-export([read/2, write/2]).
-include("map.hrl").


read(20001, <<>>) ->
    {ok, []};

read(20007, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(20008, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, [List]) ->
    {ok, protocol:pack(20001, <<(length(List)):16, <<<<Id:64, Type:8, X:16, Y:16>> || #fighter{id = Id, type = Type, x = X, y = Y} <- List>>/binary>>)};

write(20007, [#fighter{id = Id, x = X, y = Y}]) ->
    {ok, protocol:pack(20007, <<Id:64, X:16, Y:16>>)};

write(20008, [#fighter{id = Id, x = X, y = Y}]) ->
    {ok, protocol:pack(20008, <<Id:64, X:16, Y:16>>)};

write(Code, Content) ->
    {error, Code, Content}.
