-module(fashion_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 12001, {}) ->
    fashion:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = fashion_protocol:encode(12001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

