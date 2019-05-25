-module(protocol_13).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


read(13001, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(13001, [List]) ->
    ListBinary = <<(length(List)):16, <<<<QuestId:32, GroupId:32, (length(Progress)):16, <<<<Id:16, Value:16>> || #quest_progress{id = Id, value = Value} <- Progress>>/binary, Award:8>> || #quest{quest_id = QuestId, group_id = GroupId, progress = Progress, award = Award} <- List>>/binary>>,
    {ok, protocol:pack(13001, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
