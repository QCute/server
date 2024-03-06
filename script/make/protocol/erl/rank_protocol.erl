-module(rank_protocol).
-export([decode/2, encode/2]).
-include("rank.hrl").
-include("rank.hrl").
-include("rank.hrl").
-include("rank.hrl").
-include("rank.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(19001, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(19002, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(19003, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(19004, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(19005, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(19001, List) ->
    Data19001 = <<(encode_list_19001(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data19001)):16, 19001:16, Data19001/binary>>};

encode(19002, List) ->
    Data19002 = <<(encode_list_19002(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data19002)):16, 19002:16, Data19002/binary>>};

encode(19003, List) ->
    Data19003 = <<(encode_list_19003(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data19003)):16, 19003:16, Data19003/binary>>};

encode(19004, List) ->
    Data19004 = <<(encode_list_19004(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data19004)):16, 19004:16, Data19004/binary>>};

encode(19005, List) ->
    Data19005 = <<(encode_list_19005(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data19005)):16, 19005:16, Data19005/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_19001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_19001(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId} | List]) ->
    encode_list_19001(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16>>, Length + 1, List).

encode_list_19002(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_19002(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes}} | List]) ->
    encode_list_19002(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8>>, Length + 1, List).

encode_list_19003(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_19003(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex}} | List]) ->
    encode_list_19003(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8>>, Length + 1, List).

encode_list_19004(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_19004(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel}} | List]) ->
    encode_list_19004(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8>>, Length + 1, List).

encode_list_19005(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_19005(Acc = <<_/binary>>, Length, [#rank{type = Type, order = Order, key = Key, value = Value, time = Time, name = Name, server_id = ServerId, other = {Level, Classes, Sex, VipLevel, Avatar}} | List]) ->
    encode_list_19005(<<Acc/binary, Type:16, Order:64, Key:64, Value:64, Time:32, (byte_size(Name)):16, (Name)/binary, ServerId:16, Level:16, Classes:8, Sex:8, VipLevel:8, Avatar:8>>, Length + 1, List).

