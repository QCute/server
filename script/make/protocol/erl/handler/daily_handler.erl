-module(daily_handler).
-export([handle/3]).
-export([send_query_count/2]).
-export([send_query/3]).
-export([send_award/2]).
-export([send_award_active/2]).
-include("user.hrl").

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

send_query_count(User, List) ->
    {ok, Binary} = daily_protocol:encode(12301, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query(User, List, DailyActive) ->
    {ok, Binary} = daily_protocol:encode(12302, [List, DailyActive]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award(User, Result) ->
    {ok, Binary} = daily_protocol:encode(12303, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award_active(User, Result) ->
    {ok, Binary} = daily_protocol:encode(12304, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

