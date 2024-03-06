-module(daily_protocol).
-export([decode/2, encode/2]).
-include("count.hrl").
-include("daily.hrl").
-include("daily.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12301, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(12302, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(12303, _Rest_ = <<_/binary>>) ->
    <<DailyId:32, _DailyIdRest_/binary>> = _Rest_,
    {ok, DailyId};

decode(12304, _Rest_ = <<_/binary>>) ->
    <<StageId:32, _StageIdRest_/binary>> = _Rest_,
    {ok, StageId};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12301, List) ->
    Data12301 = <<(encode_list_12301(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data12301)):16, 12301:16, Data12301/binary>>};

encode(12302, [List, #daily_active{stage_id = StageId, score = Score}]) ->
    Data12302 = <<(encode_list_12302(<<>>, 0, List))/binary, StageId:32, Score:32>>,
    {ok, <<(byte_size(Data12302)):16, 12302:16, Data12302/binary>>};

encode(12303, Result) ->
    Data12303 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data12303)):16, 12303:16, Data12303/binary>>};

encode(12304, Result) ->
    Data12304 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data12304)):16, 12304:16, Data12304/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_12301(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12301(Acc = <<_/binary>>, Length, [#count{type = Type, today_number = TodayNumber} | List]) ->
    encode_list_12301(<<Acc/binary, Type:32, TodayNumber:32>>, Length + 1, List).

encode_list_12302(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12302(Acc = <<_/binary>>, Length, [#daily{daily_id = DailyId, is_award = IsAward} | List]) ->
    encode_list_12302(<<Acc/binary, DailyId:32, IsAward:8>>, Length + 1, List).

