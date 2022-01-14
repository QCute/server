-module(auction_handler).
-export([handle/3]).

handle(_, 16101, []) ->
    auction_server:query();

handle(User, 16102, [AuctionNo, NextPrice]) ->
    auction_server:bid(User, AuctionNo, NextPrice);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
