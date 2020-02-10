-module(dungeon_protocol).
-export([read/2, write/2]).
-include("dungeon.hrl").


read(17001, <<>>) ->
    {ok, []};

read(17002, <<DungeonId:32>>) ->
    {ok, DungeonId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(17001, List) ->
    {ok, protocol:pack(17001, <<(length(List)):16, <<<<DungeonId:32, TodayNumber:16, TotalNumber:16>> || #dungeon{dungeon_id = DungeonId, today_number = TodayNumber, total_number = TotalNumber} <- List>>/binary>>)};

write(17002, Result) ->
    {ok, protocol:pack(17002, <<(text(17002, Result))/binary>>)};

write(17003, Result) ->
    {ok, protocol:pack(17003, <<(text(17003, Result))/binary>>)};

write(17004, Result) ->
    {ok, protocol:pack(17004, <<(text(17004, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(17002, condition_not_met, sc) ->
    <<15:16, "条件不满足"/utf8>>;
text(17002, configure_not_found, sc) ->
    <<12:16, "配置错误"/utf8>>;
text(17002, item_not_enough, sc) ->
    <<18:16, "消耗材料不足"/utf8>>;
text(17002, today_number_limit, sc) ->
    <<33:16, "今天进入次数已达到上限"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

