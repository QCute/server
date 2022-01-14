-module(auction_protocol).
-export([read/2, write/2]).
-include("auction.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(16101, <<>>) ->
    {ok, []};

read(16102, <<AuctionNo:64, NextPrice:32>>) ->
    {ok, [AuctionNo, NextPrice]};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(16101, List) ->
    ListBinary = protocol:write_ets(fun([#auction{auction_no = AuctionNo, auction_id = AuctionId, number = Number, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}]) -> <<AuctionNo:64, AuctionId:32, Number:16, Type:8, EndTime:32, NowPrice:32, NextPrice:32>> end, List),
    {ok, protocol:pack(16101, <<ListBinary/binary>>)};

write(16102, [Result, NewPrice, #auction{auction_no = AuctionNo, auction_id = AuctionId, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}]) ->
    {ok, protocol:pack(16102, <<(protocol:text(Result))/binary, NewPrice:32, AuctionNo:64, AuctionId:32, Type:8, EndTime:32, NowPrice:32, NextPrice:32>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


