-module(auction_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").


get(1) ->
    #auction_data{auction_id = 1, auction_type = 1, begin_price = 1, add_price = 1, tax = 0, show_time = 60, auction_time = 120, critical_time = 120, overtime = 120};
get(2) ->
    #auction_data{auction_id = 2, auction_type = 2, begin_price = 2, add_price = 2, tax = 0, show_time = 60, auction_time = 120, critical_time = 120, overtime = 120};
get(_) ->
    [].


