-module(quest_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


read(11201, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11201, [List]) ->
    {ok, protocol:pack(11201, <<(length(List)):16, <<<<QuestId:32, Amount:16, Award:8>> || #quest{quest_id = QuestId, amount = Amount, award = Award} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
