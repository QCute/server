-module(auction_sql).
-export([save/1]).
-export([select/0]).
-export([delete/1]).
-include("auction.hrl").

%% @doc insert into auction
-spec save(AuctionList :: [#auction{}] | ets:tab()) -> NewAuctionList :: [#auction{}].
save(AuctionList) ->
    db:save(<<"INSERT INTO `auction` (`auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, AuctionList, fun(#auction{auction_no = AuctionNo, auction_id = AuctionId, number = Number, type = Type, bid_type = BidType, start_time = StartTime, end_time = EndTime, from = From, bid_number = BidNumber, now_price = NowPrice, next_price = NextPrice, guild_id = GuildId}) -> [AuctionNo, AuctionId, Number, Type, BidType, StartTime, EndTime, From, BidNumber, NowPrice, NextPrice, GuildId] end, #auction.flag).

%% @doc select from auction
-spec select() -> Rows :: [#auction{}].
select() ->
    Data = db:select(<<"SELECT `auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id` FROM `auction`">>, []),
    parser:convert(Data, auction).

%% @doc delete row from auction
-spec delete(AuctionNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(AuctionNo) ->
    db:delete(<<"DELETE FROM `auction` WHERE `auction_no` = ?">>, [AuctionNo]).
