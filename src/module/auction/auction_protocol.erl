-module(auction_protocol).
-export([decode/2, encode/2]).
-include("auction.hrl").


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(16101, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(16102, _Rest_ = <<_/binary>>) ->
    <<AuctionNo:64, _AuctionNoRest_/binary>> = _Rest_,
    <<NextPrice:32, _NextPriceRest_/binary>> = _AuctionNoRest_,
    {ok, [AuctionNo, NextPrice]};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(16101, List) ->
    Data16101 = <<(encode_list_16101(<<>>, 0, ets:safe_fixtable(List, true) andalso List, ets:first(List)))/binary>>,
    {ok, <<(byte_size(Data16101)):16, 16101:16, Data16101/binary>>};

encode(16102, [Result, NewPrice, #auction{auction_no = AuctionNo, auction_id = AuctionId, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}]) ->
    Data16102 = <<(protocol:text(Result))/binary, NewPrice:32, AuctionNo:64, AuctionId:32, Type:8, EndTime:32, NowPrice:32, NextPrice:32>>,
    {ok, <<(byte_size(Data16102)):16, 16102:16, Data16102/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_16101(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
encode_list_16101(Acc = <<_/binary>>, Length, Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            encode_list_16101(Acc, Length, Tab, ets:next(Tab, Key));
        [#auction{auction_no = AuctionNo, auction_id = AuctionId, number = Number, type = Type, end_time = EndTime, now_price = NowPrice, next_price = NextPrice}] ->
            encode_list_16101(<<Acc/binary, AuctionNo:64, AuctionId:32, Number:16, Type:8, EndTime:32, NowPrice:32, NextPrice:32>>, Length + 1, Tab, ets:next(Tab, Key))
    end.

