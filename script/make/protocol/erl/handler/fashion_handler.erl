-module(fashion_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 12001, []) ->
    fashion:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = fashion_protocol:encode(12001, List),
    User#user{buffer = [Binary | User#user.buffer]}.

