-module(fashion_protocol).
-export([read/2, write/2]).
-include("fashion.hrl").


read(12001, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(12001, List) ->
    ListBinary = protocol:write_list(fun(#fashion{fashion_id = FashionId, expire_time = ExpireTime}) -> <<FashionId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(12001, <<ListBinary/binary>>)};

write(12002, List) ->
    ListBinary = protocol:write_list(fun(#fashion{fashion_id = FashionId}) -> <<FashionId:32>> end, List),
    {ok, protocol:pack(12002, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

