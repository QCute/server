-module(protocol_20).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


read(56789, <<QuestId:32>>) ->
    {ok, [QuestId]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(56789, [#quest{quest_id = QuestId, progress = Progress}]) ->
    QuestBinary = <<QuestId:32, (length(Progress)):16, <<<<Id:8, Value:32>> || #quest_progress{id = Id, value = Value} <- Progress>>>>,
    {ok, protocol:pack(56789, <<QuestBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
