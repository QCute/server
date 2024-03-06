-module(shop_protocol).
-export([decode/2, encode/2]).
-include("shop.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11301, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11302, _Rest_ = <<_/binary>>) ->
    <<ShopId:32, _ShopIdRest_/binary>> = _Rest_,
    <<Number:16, _NumberRest_/binary>> = _ShopIdRest_,
    {ok, {ShopId, Number}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11301, Data) ->
    Data11301 = <<(encode_data_11301(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11301)):16, 11301:16, Data11301/binary>>};

encode(11302, Data) ->
    Data11302 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data11302)):16, 11302:16, Data11302/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_11301(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11301(Acc = <<_/binary>>, Length, [#shop{shop_id = ShopId, number = Number} | Data]) ->
    encode_data_11301(<<Acc/binary, ShopId:32, Number:16>>, Length + 1, Data).

