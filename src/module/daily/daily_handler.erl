-module(daily_handler).
-export([handle/3]).

handle(User, 12301, []) ->
    daily:query_count(User);

handle(User, 12302, []) ->
    daily:query(User);

handle(User, 12303, DailyId) ->
    daily:award(User, DailyId);

handle(User, 12304, StageId) ->
    daily:award_active(User, StageId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
