-module(fashion_protocol).
-export([read/2, write/2]).
-include("fashion.hrl").


read(12001, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(12001, List) ->
    {ok, protocol:pack(12001, <<(length(List)):16, <<<<FashionId:32, ExpireTime:32>> || #fashion{fashion_id = FashionId, expire_time = ExpireTime} <- List>>/binary>>)};

write(12002, List) ->
    {ok, protocol:pack(12002, <<(length(List)):16, <<<<FashionId:32>> || #fashion{fashion_id = FashionId} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

