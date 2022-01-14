-module(notice_protocol).
-export([read/2, write/2]).


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(50001, [Scope, Type, Title, Msg]) ->
    {ok, protocol:pack(50001, <<Scope:8, Type:8, (byte_size(Title)):16, (Title)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


