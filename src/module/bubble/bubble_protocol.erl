-module(bubble_protocol).
-export([read/2, write/2]).
-include("bubble.hrl").


read(12101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(12101, List) ->
    ListBinary = protocol:write_list(fun(#bubble{bubble_id = BubbleId, expire_time = ExpireTime}) -> <<BubbleId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(12101, <<ListBinary/binary>>)};

write(12102, List) ->
    ListBinary = protocol:write_list(fun(#bubble{bubble_id = BubbleId}) -> <<BubbleId:32>> end, List),
    {ok, protocol:pack(12102, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

