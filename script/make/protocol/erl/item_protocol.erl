-module(item_protocol).
-export([decode/2, encode/2]).
-include("item.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11101, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11102, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11103, _Rest_ = <<_/binary>>) ->
    {ok, {}};


decode(11106, _Rest_ = <<_/binary>>) ->
    <<ItemNo:64, _ItemNoRest_/binary>> = _Rest_,
    <<ItemId:32, _ItemIdRest_/binary>> = _ItemNoRest_,
    <<Number:16, _NumberRest_/binary>> = _ItemIdRest_,
    <<Type:8, _TypeRest_/binary>> = _NumberRest_,
    {ok, {ItemNo, ItemId, Number, Type}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11101, ) ->
    Data11101 = <<(encode__11101(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data11101)):16, 11101:16, Data11101/binary>>};

encode(11102, ) ->
    Data11102 = <<(encode__11102(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data11102)):16, 11102:16, Data11102/binary>>};

encode(11103, ) ->
    Data11103 = <<(encode__11103(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data11103)):16, 11103:16, Data11103/binary>>};

encode(11104, ) ->
    Data11104 = <<(encode__11104(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data11104)):16, 11104:16, Data11104/binary>>};

encode(11106, ) ->
    Data11106 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data11106)):16, 11106:16, Data11106/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__11101(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__11101(Acc = <<_/binary>>, Length, [#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} | ]) ->
    encode__11101(<<Acc/binary, ItemNo:64, ItemId:64, Type:8, Number:16>>, Length + 1, ).

encode__11102(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__11102(Acc = <<_/binary>>, Length, [#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} | ]) ->
    encode__11102(<<Acc/binary, ItemNo:64, ItemId:64, Type:8, Number:16>>, Length + 1, ).

encode__11103(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__11103(Acc = <<_/binary>>, Length, [#item{item_no = ItemNo, item_id = ItemId, type = Type, number = Number} | ]) ->
    encode__11103(<<Acc/binary, ItemNo:64, ItemId:64, Type:8, Number:16>>, Length + 1, ).

encode__11104(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__11104(Acc = <<_/binary>>, Length, [#item{item_no = ItemNo, item_id = ItemId, type = Type} | ]) ->
    encode__11104(<<Acc/binary, ItemNo:64, ItemId:32, Type:8>>, Length + 1, ).

