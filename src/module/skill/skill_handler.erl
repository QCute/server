-module(skill_handler).
-export([handle/3]).

handle(User, 11701, []) ->
    skill:query(User);

handle(User, 11702, SkillId) ->
    skill:learn(User, SkillId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
