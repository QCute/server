-module(daily_handler).
-export([handle/3]).

handle(12301, User, []) ->
    daily:query_count(User);

handle(12302, User, []) ->
    daily:query(User);

handle(12303, User, DailyId) ->
    daily:award(User, DailyId);

handle(12304, User, StageId) ->
    daily:award_active(User, StageId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
