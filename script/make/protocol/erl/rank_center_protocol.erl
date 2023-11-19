-module(rank_center_protocol).
-export([decode/2, encode/2]).
-include("rank.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(19101, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19102, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19103, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19104, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19105, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(19101, Data) ->
    Data19101 = <<(encode_data_19101(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19101)):16, 19101:16, Data19101/binary>>};

encode(19102, Data) ->
    Data19102 = <<(encode_data_19102(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19102)):16, 19102:16, Data19102/binary>>};

encode(19103, Data) ->
    Data19103 = <<(encode_data_19103(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19103)):16, 19103:16, Data19103/binary>>};

encode(19104, Data) ->
    Data19104 = <<(encode_data_19104(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19104)):16, 19104:16, Data19104/binary>>};

encode(19105, Data) ->
    Data19105 = <<(encode_data_19105(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19105)):16, 19105:16, Data19105/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_19101(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19101(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | Data]) ->
    encode_data_19101(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, Data).

encode_data_19102(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19102(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses}} | Data]) ->
    encode_data_19102(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8>>, Length + 1, Data).

encode_data_19103(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19103(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex}} | Data]) ->
    encode_data_19103(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8>>, Length + 1, Data).

encode_data_19104(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19104(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel}} | Data]) ->
    encode_data_19104(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8>>, Length + 1, Data).

encode_data_19105(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19105(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel, OtherAvatar}} | Data]) ->
    encode_data_19105(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8, OtherAvatar:8>>, Length + 1, Data).

