-module(auction_protocol).
-export([read/2, write/2]).
-include("auction.hrl").


read(16101, <<>>) ->
    {ok, []};

read(16102, <<AuctionNo:64, NextPrice:32>>) ->
    {ok, [AuctionNo, NextPrice]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(16101, List) ->
    ListBinary = protocol:write_ets(fun([#auction{auction_no = AuctionNo, auction_id = AuctionId, number = Number, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}]) -> <<AuctionNo:64, AuctionId:32, Number:16, Type:8, EndTime:32, NowPrice:32, NextPrice:32>> end, List),
    {ok, protocol:pack(16101, <<ListBinary/binary>>)};

write(16102, [Result, NewPrice, #auction{auction_no = AuctionNo, auction_id = AuctionId, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}]) ->
    {ok, protocol:pack(16102, <<(protocol:text(16102, Result))/binary, NewPrice:32, AuctionNo:64, AuctionId:32, Type:8, EndTime:32, NowPrice:32, NextPrice:32>>)};

write(Code, Content) ->
    {error, Code, Content}.

