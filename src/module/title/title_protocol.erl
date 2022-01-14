-module(title_protocol).
-export([read/2, write/2]).
-include("title.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11901, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11901, List) ->
    ListBinary = protocol:write_list(fun(#title{title_id = TitleId, expire_time = ExpireTime}) -> <<TitleId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(11901, <<ListBinary/binary>>)};

write(11902, List) ->
    ListBinary = protocol:write_list(fun(#title{title_id = TitleId}) -> <<TitleId:32>> end, List),
    {ok, protocol:pack(11902, <<ListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


