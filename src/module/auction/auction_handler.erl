-module(auction_handler).
-export([handle/3]).

handle(16101, _, []) ->
    auction_server:query();

handle(16102, User, [UniqueId]) ->
    auction_server:bid(User, UniqueId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
