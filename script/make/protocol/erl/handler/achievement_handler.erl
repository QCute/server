-module(achievement_handler).
-export([handle/3]).
-export([send_query_count/2]).
-export([send_query/2]).
-export([send_award/2]).
-include("user.hrl").

handle(User, 12201, {}) ->
    achievement:query_count(User);

handle(User, 12202, {}) ->
    achievement:query(User);

handle(User, 12203, Data) ->
    achievement:award(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query_count(User, Data) ->
    {ok, Binary} = achievement_protocol:encode(12201, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query(User, Data) ->
    {ok, Binary} = achievement_protocol:encode(12202, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_award(User, Data) ->
    {ok, Binary} = achievement_protocol:encode(12203, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

