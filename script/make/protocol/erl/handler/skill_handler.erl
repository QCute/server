-module(skill_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_learn/2]).
-include("user.hrl").

handle(User, 11701, {}) ->
    skill:query(User);

handle(User, 11702, Data) ->
    skill:learn(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = skill_protocol:encode(11701, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_learn(User, Data) ->
    {ok, Binary} = skill_protocol:encode(11702, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

