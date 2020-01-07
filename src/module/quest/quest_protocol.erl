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
    {ok, protocol:pack(11201, <<(length(List)):16, <<<<QuestId:32, Number:16, Award:8>> || #quest{quest_id = QuestId, number = Number, award = Award} <- List>>/binary>>)};

write(11202, [Result, #quest{quest_id = QuestId, number = Number, award = Award}]) ->
    {ok, protocol:pack(11202, <<(text(11202, Result))/binary, QuestId:32, Number:16, Award:8>>)};

write(11203, Result) ->
    {ok, protocol:pack(11203, <<(text(11203, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(11202, condition_not_enough, sc) ->
    <<15:16, "条件不满足"/utf8>>;
text(11202, configure_not_found, sc) ->
    <<12:16, "配置错误"/utf8>>;
text(11202, no_such_quest, sc) ->
    <<15:16, "没有此任务"/utf8>>;
text(11202, not_next_quest, sc) ->
    <<18:16, "请按顺序完成"/utf8>>;
text(11202, pre_quest_not_complete, sc) ->
    <<24:16, "前置任务还没完成"/utf8>>;
text(11203, configure_not_found, sc) ->
    <<12:16, "配置错误"/utf8>>;
text(11203, no_such_quest, sc) ->
    <<15:16, "没有此任务"/utf8>>;
text(11203, quest_already_submit, sc) ->
    <<15:16, "任务已提交"/utf8>>;
text(11203, quest_not_complete, sc) ->
    <<18:16, "任务还没完成"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

