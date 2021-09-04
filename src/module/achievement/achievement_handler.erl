-module(achievement_handler).
-export([handle/3]).

handle(12301, User, []) ->
    achievement:query_count(User);

handle(12202, User, []) ->
    achievement:query(User);

handle(12203, User, AchievementId) ->
    achievement:award(User, AchievementId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
