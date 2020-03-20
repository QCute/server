-module(auction_handler).
-export([handle/3]).

handle(16101, _, []) ->
    auction_server:query();

handle(16102, User, [AuctionNo, NextPrice]) ->
    auction_server:bid(User, AuctionNo, NextPrice);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
