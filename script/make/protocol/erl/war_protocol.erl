-module(war_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(18001, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(18001, Data) ->
    Data18001 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data18001)):16, 18001:16, Data18001/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

