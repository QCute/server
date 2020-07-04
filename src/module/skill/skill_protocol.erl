-module(skill_protocol).
-export([read/2, write/2]).
-include("skill.hrl").


read(11701, <<>>) ->
    {ok, []};

read(11702, <<SkillId:32>>) ->
    {ok, SkillId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11701, List) ->
    {ok, protocol:pack(11701, <<(length(List)):16, <<<<SkillId:32, Level:16>> || #skill{skill_id = SkillId, level = Level} <- List>>/binary>>)};

write(11702, Result) ->
    {ok, protocol:pack(11702, <<(protocol:text(11702, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

