-module(fashion_protocol).
-export([decode/2, encode/2]).
-include("fashion.hrl").
-include("fashion.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12001, _Rest_ = <<_/binary>>) ->
    {ok, []};


decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12001, List) ->
    Data12001 = <<(encode_list_12001(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data12001)):16, 12001:16, Data12001/binary>>};

encode(12002, List) ->
    Data12002 = <<(encode_list_12002(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data12002)):16, 12002:16, Data12002/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_12001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12001(Acc = <<_/binary>>, Length, [#fashion{fashion_id = FashionId, expire_time = ExpireTime} | List]) ->
    encode_list_12001(<<Acc/binary, FashionId:32, ExpireTime:32>>, Length + 1, List).

encode_list_12002(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12002(Acc = <<_/binary>>, Length, [#fashion{fashion_id = FashionId} | List]) ->
    encode_list_12002(<<Acc/binary, FashionId:32>>, Length + 1, List).

