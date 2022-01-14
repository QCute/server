-module(achievement_handler).
-export([handle/3]).

handle(User, 12301, []) ->
    achievement:query_count(User);

handle(User, 12202, []) ->
    achievement:query(User);

handle(User, 12203, AchievementId) ->
    achievement:award(User, AchievementId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
