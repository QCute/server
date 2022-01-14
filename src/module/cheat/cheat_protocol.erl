-module(cheat_protocol).
-export([read/2, write/2]).


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(60001, <<>>) ->
    {ok, []};

read(60002, <<CommandLength:16, Command:CommandLength/binary>>) ->
    {ok, Command};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(60001, CheatList) ->
    CheatListBinary = protocol:write_list(fun({Description, Command}) -> <<(byte_size(Description)):16, (Description)/binary, (byte_size(Command)):16, (Command)/binary>> end, CheatList),
    {ok, protocol:pack(60001, <<CheatListBinary/binary>>)};

write(60002, Result) ->
    {ok, protocol:pack(60002, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


