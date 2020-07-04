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
    {ok, protocol:pack(17002, <<(protocol:text(17002, Result))/binary>>)};

write(17003, Result) ->
    {ok, protocol:pack(17003, <<(protocol:text(17003, Result))/binary>>)};

write(17004, Result) ->
    {ok, protocol:pack(17004, <<(protocol:text(17004, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

