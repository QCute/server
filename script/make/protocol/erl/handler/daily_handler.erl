-module(daily_handler).
-export([handle/3]).
-export([send_query_active/2]).
-export([send_query/2]).
-export([send_award/2]).
-export([send_award_active/2]).
-include("user.hrl").

handle(User, 12301, {}) ->
    daily:query_active(User);

handle(User, 12302, {}) ->
    daily:query(User);

handle(User, 12303, Data) ->
    daily:award(User, Data);

handle(User, 12304, Data) ->
    daily:award_active(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query_active(User, Data) ->
    {ok, Binary} = daily_protocol:encode(12301, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query(User, Data) ->
    {ok, Binary} = daily_protocol:encode(12302, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award(User, Data) ->
    {ok, Binary} = daily_protocol:encode(12303, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award_active(User, Data) ->
    {ok, Binary} = daily_protocol:encode(12304, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

