-module(quest_protocol).
-export([read/2, write/2]).
-include("quest.hrl").


read(11201, <<>>) ->
    {ok, []};

read(11202, <<QuestId:8>>) ->
    {ok, [QuestId]};

read(11203, <<QuestId:8>>) ->
    {ok, [QuestId]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11201, [List]) ->
    {ok, protocol:pack(11201, <<(length(List)):16, <<<<QuestId:32, Amount:16, Award:8>> || #quest{quest_id = QuestId, amount = Amount, award = Award} <- List>>/binary>>)};

write(11202, [Result, #quest{quest_id = QuestId, amount = Amount, award = Award}]) ->
    {ok, protocol:pack(11202, <<Result:8, QuestId:32, Amount:16, Award:8>>)};

write(11203, [Result]) ->
    {ok, protocol:pack(11203, <<Result:8>>)};

write(Code, Content) ->
    {error, Code, Content}.
