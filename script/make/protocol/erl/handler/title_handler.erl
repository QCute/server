-module(title_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 11901, {}) ->
    title:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = title_protocol:encode(11901, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

