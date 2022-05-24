-module(notice_handler).
-export([handle/3]).

handle(User, 50001, []) ->
    notice:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
