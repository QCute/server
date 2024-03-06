-module(bubble_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 12101, []) ->
    bubble:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = bubble_protocol:encode(12101, List),
    User#user{buffer = [Binary | User#user.buffer]}.

