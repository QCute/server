-module(daily_protocol).
-export([read/2, write/2]).
-include("count.hrl").
-include("daily.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(12301, <<>>) ->
    {ok, []};

read(12302, <<>>) ->
    {ok, []};

read(12303, <<DailyId:32>>) ->
    {ok, DailyId};

read(12304, <<StageId:32>>) ->
    {ok, StageId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(12301, List) ->
    ListBinary = protocol:write_list(fun(#count{type = Type, today_number = TodayNumber}) -> <<Type:32, TodayNumber:32>> end, List),
    {ok, protocol:pack(12301, <<ListBinary/binary>>)};

write(12302, [List, #daily_active{stage_id = StageId, score = Score}]) ->
    ListBinary = protocol:write_list(fun(#daily{daily_id = DailyId, is_award = IsAward}) -> <<DailyId:32, IsAward:8>> end, List),
    {ok, protocol:pack(12302, <<ListBinary/binary, StageId:32, Score:32>>)};

write(12303, Result) ->
    {ok, protocol:pack(12303, <<(protocol:text(Result))/binary>>)};

write(12304, Result) ->
    {ok, protocol:pack(12304, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


