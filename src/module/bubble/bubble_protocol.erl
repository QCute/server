-module(bubble_protocol).
-export([read/2, write/2]).
-include("bubble.hrl").


read(12101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(12101, List) ->
    {ok, protocol:pack(12101, <<(length(List)):16, <<<<BubbleId:32, ExpireTime:32>> || #bubble{bubble_id = BubbleId, expire_time = ExpireTime} <- List>>/binary>>)};

write(12102, List) ->
    {ok, protocol:pack(12102, <<(length(List)):16, <<<<BubbleId:32>> || #bubble{bubble_id = BubbleId} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

