-module(rank_world_protocol).
-export([decode/2, encode/2]).
-include("rank.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(19201, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19202, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19203, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19204, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19205, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(19201, Data) ->
    Data19201 = <<(encode_data_19201(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19201)):16, 19201:16, Data19201/binary>>};

encode(19202, Data) ->
    Data19202 = <<(encode_data_19202(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19202)):16, 19202:16, Data19202/binary>>};

encode(19203, Data) ->
    Data19203 = <<(encode_data_19203(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19203)):16, 19203:16, Data19203/binary>>};

encode(19204, Data) ->
    Data19204 = <<(encode_data_19204(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19204)):16, 19204:16, Data19204/binary>>};

encode(19205, Data) ->
    Data19205 = <<(encode_data_19205(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19205)):16, 19205:16, Data19205/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_19201(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19201(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | Data]) ->
    encode_data_19201(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, Data).

encode_data_19202(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19202(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses}} | Data]) ->
    encode_data_19202(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8>>, Length + 1, Data).

encode_data_19203(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19203(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex}} | Data]) ->
    encode_data_19203(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8>>, Length + 1, Data).

encode_data_19204(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19204(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel}} | Data]) ->
    encode_data_19204(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8>>, Length + 1, Data).

encode_data_19205(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19205(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel, OtherAvatar}} | Data]) ->
    encode_data_19205(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8, OtherAvatar:8>>, Length + 1, Data).

