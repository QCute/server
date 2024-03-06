-module(cheat_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(60001, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(60002, _Rest_ = <<_/binary>>) ->
    <<Length:16, :Length/binary, _Rest_/binary>> = _Rest_,
    {ok, };

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(60001, ) ->
    Data60001 = <<(encode__60001(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data60001)):16, 60001:16, Data60001/binary>>};

encode(60002, ) ->
    Data60002 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data60002)):16, 60002:16, Data60002/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__60001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__60001(Acc = <<_/binary>>, Length, [{Description, Command} | ]) ->
    encode__60001(<<Acc/binary, (byte_size(Description)):16, (Description)/binary, (byte_size(Command)):16, (Command)/binary>>, Length + 1, ).

