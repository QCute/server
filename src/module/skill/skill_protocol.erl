-module(skill_protocol).
-export([read/2, write/2]).
-include("skill.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11701, <<>>) ->
    {ok, []};

read(11702, <<SkillId:32>>) ->
    {ok, SkillId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11701, List) ->
    ListBinary = protocol:write_list(fun(#skill{skill_id = SkillId, level = Level}) -> <<SkillId:32, Level:16>> end, List),
    {ok, protocol:pack(11701, <<ListBinary/binary>>)};

write(11702, Result) ->
    {ok, protocol:pack(11702, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


