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
encode(19001, ) ->
    Data19001 = <<(encode__19001(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19001)):16, 19001:16, Data19001/binary>>};

encode(19002, ) ->
    Data19002 = <<(encode__19002(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19002)):16, 19002:16, Data19002/binary>>};

encode(19003, ) ->
    Data19003 = <<(encode__19003(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19003)):16, 19003:16, Data19003/binary>>};

encode(19004, ) ->
    Data19004 = <<(encode__19004(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19004)):16, 19004:16, Data19004/binary>>};

encode(19005, ) ->
    Data19005 = <<(encode__19005(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data19005)):16, 19005:16, Data19005/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__19001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19001(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | ]) ->
    encode__19001(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, ).

encode__19002(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19002(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses}} | ]) ->
    encode__19002(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8>>, Length + 1, ).

encode__19003(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19003(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex}} | ]) ->
    encode__19003(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8>>, Length + 1, ).

encode__19004(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19004(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel}} | ]) ->
    encode__19004(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8>>, Length + 1, ).

encode__19005(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__19005(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {OtherLevel, OtherClasses, OtherSex, OtherVipLevel, OtherAvatar}} | ]) ->
    encode__19005(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, OtherLevel:16, OtherClasses:8, OtherSex:8, OtherVipLevel:8, OtherAvatar:8>>, Length + 1, ).

