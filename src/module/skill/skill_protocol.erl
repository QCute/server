-module(skill_protocol).
-export([decode/2, encode/2]).
-include("skill.hrl").


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11701, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(11702, _Rest_ = <<_/binary>>) ->
    <<SkillId:32, _SkillIdRest_/binary>> = _Rest_,
    {ok, SkillId};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11701, List) ->
    Data11701 = <<(encode_list_11701(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11701)):16, 11701:16, Data11701/binary>>};

encode(11702, Result) ->
    Data11702 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data11702)):16, 11702:16, Data11702/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_11701(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11701(Acc = <<_/binary>>, Length, [#skill{skill_id = SkillId, level = Level} | List]) ->
    encode_list_11701(<<Acc/binary, SkillId:32, Level:16>>, Length + 1, List).

