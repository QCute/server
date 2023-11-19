-module(buff_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 11801, {}) ->
    buff:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = buff_protocol:encode(11801, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

