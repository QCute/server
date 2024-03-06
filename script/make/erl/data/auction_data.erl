-module(auction_data).
-export([get/1]).
-include("auction.hrl").

-spec get(AuctionId :: non_neg_integer()) -> #auction_data{}.
get(1) ->
    #auction_data{auction_id = 1, bid_type = 1, begin_price = 1, add_price = 1, tax = 0, show_time = 0, auction_time = 0, critical_time = 0, overtime = 0};
get(_) ->
    undefined.

