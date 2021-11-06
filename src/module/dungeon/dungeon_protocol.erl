-module(dungeon_protocol).
-export([read/2, write/2]).
-include("dungeon.hrl").


read(17001, <<>>) ->
    {ok, []};

read(17002, <<DungeonId:32>>) ->
    {ok, DungeonId};

read(17005, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.


write(17001, List) ->
    ListBinary = protocol:write_list(fun(#dungeon{dungeon_id = DungeonId, today_number = TodayNumber, total_number = TotalNumber}) -> <<DungeonId:32, TodayNumber:16, TotalNumber:16>> end, List),
    {ok, protocol:pack(17001, <<ListBinary/binary>>)};

write(17002, Result) ->
    {ok, protocol:pack(17002, <<(protocol:text(Result))/binary>>)};

write(17003, Result) ->
    {ok, protocol:pack(17003, <<(protocol:text(Result))/binary>>)};

write(17004, Result) ->
    {ok, protocol:pack(17004, <<(protocol:text(Result))/binary>>)};

write(17005, Result) ->
    {ok, protocol:pack(17005, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


