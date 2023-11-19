-module(daily_protocol).
-export([decode/2, encode/2]).
-include("daily.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12301, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(12302, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(12303, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(12304, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12301, #daily_active{stage_id = StageId, score = Score}) ->
    Data12301 = <<StageId:32, Score:32>>,
    {ok, <<(byte_size(Data12301)):16, 12301:16, Data12301/binary>>};

encode(12302, Data) ->
    Data12302 = <<(encode_data_12302(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data12302)):16, 12302:16, Data12302/binary>>};

encode(12303, Data) ->
    Data12303 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data12303)):16, 12303:16, Data12303/binary>>};

encode(12304, Data) ->
    Data12304 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data12304)):16, 12304:16, Data12304/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_12302(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_12302(Acc = <<_/binary>>, Length, [#daily{daily_id = DailyId, is_award = IsAward} | Data]) ->
    encode_data_12302(<<Acc/binary, DailyId:32, IsAward:8>>, Length + 1, Data).

