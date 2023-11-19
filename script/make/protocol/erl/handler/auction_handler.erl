-module(auction_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_bid/4]).
-include("user.hrl").

handle(User, 16101, {}) ->
    auction_server:query(User);

handle(User, 16102, {AuctionNo, NextPrice}) ->
    auction_server:bid(User, AuctionNo, NextPrice);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = auction_protocol:encode(16101, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_bid(User, Result, NewPrice, Auction) ->
    {ok, Binary} = auction_protocol:encode(16102, {Result, NewPrice, Auction}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

