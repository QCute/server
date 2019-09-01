-module(rank_handler).
-export([handle/3]).

handle(19001, _, [Type]) ->
    rank_server:query(Type);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
