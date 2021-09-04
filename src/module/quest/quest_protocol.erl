-module(quest_protocol).
-export([read/2, write/2]).
-include("quest.hrl").


read(11201, <<>>) ->
    {ok, []};

read(11202, <<QuestId:32>>) ->
    {ok, QuestId};

read(11203, <<QuestId:32>>) ->
    {ok, QuestId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11201, List) ->
    ListBinary = protocol:write_list(fun(#quest{quest_id = QuestId, number = Number, is_award = IsAward}) -> <<QuestId:32, Number:16, IsAward:8>> end, List),
    {ok, protocol:pack(11201, <<ListBinary/binary>>)};

write(11202, [Result, #quest{quest_id = QuestId, number = Number, is_award = IsAward}]) ->
    {ok, protocol:pack(11202, <<(protocol:text(11202, Result))/binary, QuestId:32, Number:16, IsAward:8>>)};

write(11203, Result) ->
    {ok, protocol:pack(11203, <<(protocol:text(11203, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

