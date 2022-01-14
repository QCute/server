-module(achievement_protocol).
-export([read/2, write/2]).
-include("count.hrl").
-include("achievement.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(12301, <<>>) ->
    {ok, []};

read(12202, <<>>) ->
    {ok, []};

read(12203, <<AchievementId:32>>) ->
    {ok, AchievementId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(12301, List) ->
    ListBinary = protocol:write_list(fun(#count{type = Type, total_number = TotalNumber}) -> <<Type:32, TotalNumber:32>> end, List),
    {ok, protocol:pack(12301, <<ListBinary/binary>>)};

write(12202, List) ->
    ListBinary = protocol:write_list(fun(#achievement{achievement_id = AchievementId, type = Type}) -> <<AchievementId:32, Type:32>> end, List),
    {ok, protocol:pack(12202, <<ListBinary/binary>>)};

write(12203, Result) ->
    {ok, protocol:pack(12203, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


