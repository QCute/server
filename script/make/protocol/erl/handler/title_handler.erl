-module(title_handler).
-export([handle/3]).
-export([send_query/2]).
-include("user.hrl").

handle(User, 11901, []) ->
    title:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = title_protocol:encode(11901, List),
    User#user{buffer = [Binary | User#user.buffer]}.

