-module(auction_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").


get(1) ->
    #auction_data{
        auction_id = 1,
        auction_type = 1,
        begin_price = 1,
        add_price = 1,
        tax = 0,
        show_time = 0,
        auction_time = 0,
        critical_time = 0,
        delay_time = 0
    };
get(_) -> 
    [].

