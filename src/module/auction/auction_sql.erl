-module(auction_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").

-define(INSERT_AUCTION, <<"INSERT INTO `auction` (`auction_id`, `number`, `type`, `start_time`, `end_time`, `from`, `bid_number`, `price`, `seller_list`, `bidder_list`, `club_id`, `bidder_id`, `bidder_name`, `bidder_server_id`) VALUES ('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~s', '~w')">>).
-define(UPDATE_AUCTION, <<"UPDATE `auction` SET `auction_id` = '~w', `number` = '~w', `type` = '~w', `start_time` = '~w', `end_time` = '~w', `from` = '~w', `bid_number` = '~w', `price` = '~w', `seller_list` = '~w', `bidder_list` = '~w', `club_id` = '~w', `bidder_id` = '~w', `bidder_name` = '~s', `bidder_server_id` = '~w' WHERE `unique_id` = '~w'">>).
-define(SELECT_AUCTION, <<"SELECT * FROM `auction` ">>).
-define(DELETE_AUCTION, <<"DELETE  FROM `auction` WHERE `unique_id` = '~w'">>).
-define(UPDATE_INTO_AUCTION, {<<"INSERT INTO `auction` (`unique_id`, `auction_id`, `number`, `type`, `start_time`, `end_time`, `from`, `bid_number`, `price`, `seller_list`, `bidder_list`, `club_id`, `bidder_id`, `bidder_name`, `bidder_server_id`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `auction_id` = VALUES(`auction_id`), `number` = VALUES(`number`), `type` = VALUES(`type`), `start_time` = VALUES(`start_time`), `end_time` = VALUES(`end_time`), `from` = VALUES(`from`), `bid_number` = VALUES(`bid_number`), `price` = VALUES(`price`), `seller_list` = VALUES(`seller_list`), `bidder_list` = VALUES(`bidder_list`), `club_id` = VALUES(`club_id`), `bidder_id` = VALUES(`bidder_id`), `bidder_name` = VALUES(`bidder_name`), `bidder_server_id` = VALUES(`bidder_server_id`)">>}).

%% @doc update_into
update_into(DataList) ->
    F = fun(Auction) -> [
        Auction#auction.unique_id,
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.price,
        Auction#auction.seller_list,
        Auction#auction.bidder_list,
        Auction#auction.club_id,
        Auction#auction.bidder_id,
        Auction#auction.bidder_name,
        Auction#auction.bidder_server_id
    ] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_AUCTION, #auction.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Auction) ->
    Sql = parser:format(?INSERT_AUCTION, [
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.price,
        Auction#auction.seller_list,
        Auction#auction.bidder_list,
        Auction#auction.club_id,
        Auction#auction.bidder_id,
        Auction#auction.bidder_name,
        Auction#auction.bidder_server_id
    ]),
    sql:insert(Sql).

%% @doc update
update(Auction) ->
    Sql = parser:format(?UPDATE_AUCTION, [
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.price,
        Auction#auction.seller_list,
        Auction#auction.bidder_list,
        Auction#auction.club_id,
        Auction#auction.bidder_id,
        Auction#auction.bidder_name,
        Auction#auction.bidder_server_id,
        Auction#auction.unique_id
    ]),
    sql:update(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION, [
        
    ]),
    sql:select(Sql).

%% @doc delete
delete(UniqueId) ->
    Sql = parser:format(?DELETE_AUCTION, [
        UniqueId
    ]),
    sql:delete(Sql).

