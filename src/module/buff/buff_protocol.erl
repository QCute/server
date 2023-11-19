-module(buff_protocol).
-export([decode/2, encode/2]).
-include("buff.hrl").


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11801, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11801, List) ->
    Data11801 = <<(encode_list_11801(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11801)):16, 11801:16, Data11801/binary>>};

encode(11802, List) ->
    Data11802 = <<(encode_list_11802(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11802)):16, 11802:16, Data11802/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_11801(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11801(Acc = <<_/binary>>, Length, [#buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap} | List]) ->
    encode_list_11801(<<Acc/binary, BuffId:32, ExpireTime:32, Overlap:16>>, Length + 1, List).

encode_list_11802(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11802(Acc = <<_/binary>>, Length, [#buff{buff_id = BuffId} | List]) ->
    encode_list_11802(<<Acc/binary, BuffId:32>>, Length + 1, List).

