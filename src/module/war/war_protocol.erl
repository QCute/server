-module(war_protocol).
-export([decode/2, encode/2]).


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(18001, _Rest_ = <<_/binary>>) ->
    <<MonsterId:32, _MonsterIdRest_/binary>> = _Rest_,
    {ok, MonsterId};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(18001, Result) ->
    Data18001 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data18001)):16, 18001:16, Data18001/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

