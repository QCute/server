-module(auction_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").


read(16101, <<>>) ->
    {ok, []};

read(16102, <<Undefined1:64>>) ->
    {ok, [Undefined1]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(16101, [List]) ->
    ListBinary = protocol:write_ets(fun([#auction{unique_id = UniqueId, auction_id = AuctionId, number = Number, end_time = EndTime, price = Price, bidder_id = BidderId, bidder_name = BidderName}]) -> <<UniqueId:64, AuctionId:32, Number:16, EndTime:32, Price:32, BidderId:64, (byte_size(BidderName)):16, (BidderName)/binary>> end, List),
    {ok, protocol:pack(16101, <<ListBinary/binary>>)};

write(16102, [Result, NewPrice, #auction{unique_id = UniqueId, auction_id = AuctionId, number = Number, end_time = EndTime, price = Price, bidder_id = BidderId, bidder_name = BidderName}]) ->
    ResultBinary = <<Result:8>>,
    NewPriceBinary = <<NewPrice:32>>,
    AuctionBinary = <<UniqueId:64, AuctionId:32, Number:16, EndTime:32, Price:32, BidderId:64, (byte_size(BidderName)):16, (BidderName)/binary>>,
    {ok, protocol:pack(16102, <<ResultBinary/binary, NewPriceBinary/binary, AuctionBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
