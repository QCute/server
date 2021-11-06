-module(achievement_protocol).
-export([read/2, write/2]).
-include("count.hrl").
-include("achievement.hrl").


read(12301, <<>>) ->
    {ok, []};

read(12202, <<>>) ->
    {ok, []};

read(12203, <<AchievementId:32>>) ->
    {ok, AchievementId};

read(Code, Binary) ->
    {error, Code, Binary}.


write(12301, List) ->
    ListBinary = protocol:write_list(fun(#count{type = Type, total_number = TotalNumber}) -> <<Type:32, TotalNumber:32>> end, List),
    {ok, protocol:pack(12301, <<ListBinary/binary>>)};

write(12202, List) ->
    ListBinary = protocol:write_list(fun(#achievement{achievement_id = AchievementId, type = Type}) -> <<AchievementId:32, Type:32>> end, List),
    {ok, protocol:pack(12202, <<ListBinary/binary>>)};

write(12203, Result) ->
    {ok, protocol:pack(12203, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


