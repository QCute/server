-module(achievement_protocol).
-export([decode/2, encode/2]).
-include("count.hrl").
-include("achievement.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12201, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(12202, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(12203, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12201, Data) ->
    Data12201 = <<(encode_data_12201(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data12201)):16, 12201:16, Data12201/binary>>};

encode(12202, Data) ->
    Data12202 = <<(encode_data_12202(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data12202)):16, 12202:16, Data12202/binary>>};

encode(12203, Data) ->
    Data12203 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data12203)):16, 12203:16, Data12203/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_12201(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_12201(Acc = <<_/binary>>, Length, [#count{type = Type, total_number = TotalNumber} | Data]) ->
    encode_data_12201(<<Acc/binary, Type:32, TotalNumber:32>>, Length + 1, Data).

encode_data_12202(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_12202(Acc = <<_/binary>>, Length, [#achievement{achievement_id = AchievementId, type = Type} | Data]) ->
    encode_data_12202(<<Acc/binary, AchievementId:32, Type:32>>, Length + 1, Data).

