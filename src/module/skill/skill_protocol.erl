-module(skill_protocol).
-export([read/2, write/2]).
-include("skill.hrl").


read(11701, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11701, List) ->
    {ok, protocol:pack(11701, <<(length(List)):16, <<<<SkillId:32, Level:16>> || #skill{skill_id = SkillId, level = Level} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
