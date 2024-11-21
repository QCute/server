-module(skill_protocol).
-export([decode/2, encode/2]).
-include("skill.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11701, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11702, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11701, Data) ->
    Data11701 = <<(encode_data_11701(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11701)):16, 11701:16, Data11701/binary>>};

encode(11702, Data) ->
    Data11702 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data11702)):16, 11702:16, Data11702/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_11701(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11701(Acc = <<_/binary>>, Length, [#skill{skill_id = SkillId, level = Level} | Data]) ->
    encode_data_11701(<<Acc/binary, SkillId:32, Level:16>>, Length + 1, Data).

