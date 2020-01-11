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
    {ok, protocol:pack(11702, <<(text(11702, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(11702, condition_not_met, sc) ->
    <<12:16, "条件不足"/utf8>>;
text(11702, configure_not_found, sc) ->
    <<12:16, "配置错误"/utf8>>;
text(11702, item_not_enough, sc) ->
    <<12:16, "材料不足"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

