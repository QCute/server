-module(cheat_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(60001, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(60002, _Rest_ = <<_/binary>>) ->
    <<CommandLength:16, Command:CommandLength/binary, _CommandRest_/binary>> = _Rest_,
    {ok, Command};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(60001, CheatList) ->
    Data60001 = <<(encode_cheat_list_60001(<<>>, 0, CheatList))/binary>>,
    {ok, <<(byte_size(Data60001)):16, 60001:16, Data60001/binary>>};

encode(60002, Result) ->
    Data60002 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data60002)):16, 60002:16, Data60002/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_cheat_list_60001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_cheat_list_60001(Acc = <<_/binary>>, Length, [{Description, Command} | CheatList]) ->
    encode_cheat_list_60001(<<Acc/binary, (byte_size(Description)):16, (Description)/binary, (byte_size(Command)):16, (Command)/binary>>, Length + 1, CheatList).

