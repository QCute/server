-module(auction_sql).
-export([save/1]).
-export([select/0]).
-export([delete/1]).
-include("auction.hrl").

%% @doc insert into auction
-spec save(AuctionList :: [#auction{}] | ets:tab()) -> NewAuctionList :: [#auction{}].
save(AuctionList) ->
    db:save_into(<<"INSERT INTO `auction` (`auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:, :12:, :15:)">>, <<"ON DUPLICATE KEY UPDATE `auction_no` = VALUES(`auction_no`), `auction_id` = VALUES(`auction_id`), `number` = VALUES(`number`), `type` = VALUES(`type`), `bid_type` = VALUES(`bid_type`), `start_time` = VALUES(`start_time`), `end_time` = VALUES(`end_time`), `from` = VALUES(`from`), `bid_number` = VALUES(`bid_number`), `now_price` = VALUES(`now_price`), `next_price` = VALUES(`next_price`), `guild_id` = VALUES(`guild_id`)">>, AuctionList, #auction.flag).

%% @doc select from auction
-spec select() -> Rows :: [#auction{}].
select() ->
    Data = db:select(<<"SELECT `auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `seller_list`, `bidder_list`, `guild_id`, `timer`, `flag` FROM `auction`">>, []),
    parser:convert(Data, auction, fun(Auction = #auction{from = From}) -> Auction#auction{from = parser:to_term(From)} end).

%% @doc delete row from auction
-spec delete(AuctionNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(AuctionNo) ->
    db:delete(<<"DELETE FROM `auction` WHERE `auction_no` = ?">>, [AuctionNo]).
