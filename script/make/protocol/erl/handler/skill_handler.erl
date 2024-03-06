-module(skill_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_learn/2]).
-include("user.hrl").

handle(User, 11701, []) ->
    skill:query(User);

handle(User, 11702, SkillId) ->
    skill:learn(User, SkillId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = skill_protocol:encode(11701, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_learn(User, Result) ->
    {ok, Binary} = skill_protocol:encode(11702, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

