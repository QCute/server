-module(skill_handler).
-export([handle/3]).

handle(11701, User, []) ->
    skill:query(User);

handle(11702, User, SkillId) ->
    skill:learn(User, SkillId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
