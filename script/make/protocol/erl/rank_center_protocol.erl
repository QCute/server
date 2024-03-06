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
encode(19101, ) ->
    Data19101 = <<(encode__19101(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19101)):16, 19101:16, Data19101/binary>>};

encode(19102, ) ->
    Data19102 = <<(encode__19102(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19102)):16, 19102:16, Data19102/binary>>};

encode(19103, ) ->
    Data19103 = <<(encode__19103(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19103)):16, 19103:16, Data19103/binary>>};

encode(19104, ) ->
    Data19104 = <<(encode__19104(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19104)):16, 19104:16, Data19104/binary>>};

encode(19105, ) ->
    Data19105 = <<(encode__19105(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19105)):16, 19105:16, Data19105/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__19101(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19101(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | ]) ->
    encode__19101(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, ).

encode__19102(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19102(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses}} | ]) ->
    encode__19102(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8>>, Length + 1, ).

encode__19103(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19103(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex}} | ]) ->
    encode__19103(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8>>, Length + 1, ).

encode__19104(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19104(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel}} | ]) ->
    encode__19104(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8>>, Length + 1, ).

encode__19105(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19105(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel, OtherAvatar}} | ]) ->
    encode__19105(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8, OtherAvatar:8>>, Length + 1, ).

