-module(cheat_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(60001, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(60002, _Rest_ = <<_/binary>>) ->
    <<DataLength:16, Data:DataLength/binary, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(60001, Data) ->
    Data60001 = <<(encode_data_60001(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data60001)):16, 60001:16, Data60001/binary>>};

encode(60002, Data) ->
    Data60002 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data60002)):16, 60002:16, Data60002/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_60001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_60001(Acc = <<_/binary>>, Length, [{Description, Command} | Data]) ->
    encode_data_60001(<<Acc/binary, (byte_size(Description)):16, (Description)/binary, (byte_size(Command)):16, (Command)/binary>>, Length + 1, Data).

