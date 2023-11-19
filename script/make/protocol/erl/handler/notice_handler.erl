-module(notice_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 50001, {}) ->
    notice:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = notice_protocol:encode(50001, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

