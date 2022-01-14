-module(buff_protocol).
-export([read/2, write/2]).
-include("buff.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11801, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11801, List) ->
    ListBinary = protocol:write_list(fun(#buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap}) -> <<BuffId:32, ExpireTime:32, Overlap:16>> end, List),
    {ok, protocol:pack(11801, <<ListBinary/binary>>)};

write(11802, List) ->
    ListBinary = protocol:write_list(fun(#buff{buff_id = BuffId}) -> <<BuffId:32>> end, List),
    {ok, protocol:pack(11802, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


