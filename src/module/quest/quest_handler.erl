-module(quest_handler).
-export([handle/3]).

handle(11201, User, []) ->
    quest:query(User);

handle(11202, User, [QuestId]) ->
    quest:accept(User, QuestId);

handle(11203, User, [QuestId]) ->
    quest:submit(User, QuestId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
