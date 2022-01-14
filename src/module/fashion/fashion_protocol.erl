-module(fashion_protocol).
-export([read/2, write/2]).
-include("fashion.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(12001, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(12001, List) ->
    ListBinary = protocol:write_list(fun(#fashion{fashion_id = FashionId, expire_time = ExpireTime}) -> <<FashionId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(12001, <<ListBinary/binary>>)};

write(12002, List) ->
    ListBinary = protocol:write_list(fun(#fashion{fashion_id = FashionId}) -> <<FashionId:32>> end, List),
    {ok, protocol:pack(12002, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


