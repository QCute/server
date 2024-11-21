-module(rank_protocol).
-export([decode/2, encode/2]).
-include("rank.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(19001, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19002, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19003, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19004, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(19005, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(19001, Data) ->
    Data19001 = <<(encode_data_19001(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19001)):16, 19001:16, Data19001/binary>>};

encode(19002, Data) ->
    Data19002 = <<(encode_data_19002(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19002)):16, 19002:16, Data19002/binary>>};

encode(19003, Data) ->
    Data19003 = <<(encode_data_19003(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19003)):16, 19003:16, Data19003/binary>>};

encode(19004, Data) ->
    Data19004 = <<(encode_data_19004(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19004)):16, 19004:16, Data19004/binary>>};

encode(19005, Data) ->
    Data19005 = <<(encode_data_19005(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data19005)):16, 19005:16, Data19005/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_19001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19001(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | Data]) ->
    encode_data_19001(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, Data).

encode_data_19002(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19002(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses}} | Data]) ->
    encode_data_19002(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8>>, Length + 1, Data).

encode_data_19003(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19003(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex}} | Data]) ->
    encode_data_19003(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8>>, Length + 1, Data).

encode_data_19004(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19004(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel}} | Data]) ->
    encode_data_19004(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8>>, Length + 1, Data).

encode_data_19005(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_19005(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel, OtherAvatar}} | Data]) ->
    encode_data_19005(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8, OtherAvatar:8>>, Length + 1, Data).

