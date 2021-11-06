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
    ListBinary = protocol:write_list(fun(#skill{skill_id = SkillId, level = Level}) -> <<SkillId:32, Level:16>> end, List),
    {ok, protocol:pack(11701, <<ListBinary/binary>>)};

write(11702, Result) ->
    {ok, protocol:pack(11702, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


