-module(auction_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_bid/2]).
-include("user.hrl").

handle(User, 16101, Data) ->
    auction_server:query(User, Data);

handle(User, 16102, Data) ->
    auction_server:bid(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = auction_protocol:encode(16101, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_bid(User, Data) ->
    {ok, Binary} = auction_protocol:encode(16102, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

