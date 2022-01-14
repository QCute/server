-module(bubble_protocol).
-export([read/2, write/2]).
-include("bubble.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(12101, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(12101, List) ->
    ListBinary = protocol:write_list(fun(#bubble{bubble_id = BubbleId, expire_time = ExpireTime}) -> <<BubbleId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(12101, <<ListBinary/binary>>)};

write(12102, List) ->
    ListBinary = protocol:write_list(fun(#bubble{bubble_id = BubbleId}) -> <<BubbleId:32>> end, List),
    {ok, protocol:pack(12102, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


