-module(buff_protocol).
-export([read/2, write/2]).
-include("buff.hrl").


read(11801, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.


write(11801, List) ->
    ListBinary = protocol:write_list(fun(#buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap}) -> <<BuffId:32, ExpireTime:32, Overlap:16>> end, List),
    {ok, protocol:pack(11801, <<ListBinary/binary>>)};

write(11802, List) ->
    ListBinary = protocol:write_list(fun(#buff{buff_id = BuffId}) -> <<BuffId:32>> end, List),
    {ok, protocol:pack(11802, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


