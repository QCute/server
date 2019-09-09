-module(auction_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").
-define(INSERT_AUCTION, <<"INSERT INTO `auction` (`auction_id`, `number`, `type`, `start_time`, `end_time`, `from`, `bid_number`, `price`, `seller_list`, `bidder_list`, `club_id`, `bidder_id`, `bidder_name`, `bidder_server_id`) VALUES ('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~s', '~w')">>).
-define(SELECT_AUCTION, <<"SELECT * FROM `auction`">>).
-define(UPDATE_AUCTION, <<"UPDATE `auction` SET `auction_id` = '~w', `number` = '~w', `type` = '~w', `start_time` = '~w', `end_time` = '~w', `from` = '~w', `bid_number` = '~w', `price` = '~w', `seller_list` = '~w', `bidder_list` = '~w', `club_id` = '~w', `bidder_id` = '~w', `bidder_name` = '~s', `bidder_server_id` = '~w' WHERE `unique_id` = '~w'">>).
-define(DELETE_AUCTION, <<"DELETE  FROM `auction` WHERE `unique_id` = '~w'">>).
-define(INSERT_UPDATE_AUCTION, {<<"INSERT INTO `auction` (`auction_id`, `number`, `type`, `start_time`, `end_time`, `from`, `bid_number`, `price`, `seller_list`, `bidder_list`, `club_id`, `bidder_id`, `bidder_name`, `bidder_server_id`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `auction_id` = '~w', `number` = '~w', `type` = '~w', `start_time` = '~w', `end_time` = '~w', `from` = '~w', `bid_number` = '~w', `price` = '~w', `seller_list` = '~w', `bidder_list` = '~w', `club_id` = '~w', `bidder_id` = '~w', `bidder_name` = '~s', `bidder_server_id` = '~w'">>}).
-define(SELECT_JOIN_AUCTION, <<"SELECT `auction`.`unique_id`, `auction`.`auction_id`, `auction`.`number`, `auction`.`type`, `auction`.`start_time`, `auction`.`end_time`, `auction`.`from`, `auction`.`bid_number`, `auction`.`price`, `auction`.`seller_list`, `auction`.`bidder_list`, `auction`.`club_id`, `auction`.`bidder_id`, `auction`.`bidder_name`, `auction`.`bidder_server_id`, `auction`.`timer`, `auction`.`flag` FROM `auction`">>).
-define(DELETE_IN_UNIQUE_ID, <<"DELETE  FROM `auction` WHERE `unique_id` in (">>, <<"'~w'">>, <<")">>).

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

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION, []),
    sql:select(Sql).

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

%% @doc delete
delete(UniqueId) ->
    Sql = parser:format(?DELETE_AUCTION, [UniqueId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Auction) -> [
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
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_AUCTION, #auction.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join() ->
    Sql = parser:format(?SELECT_JOIN_AUCTION, []),
    sql:select(Sql).

%% @doc delete
delete_in_unique_id(UniqueIdList) ->
	F = fun(UniqueId) -> [UniqueId] end,
    Sql = parser:collect(UniqueIdList, F, ?DELETE_IN_UNIQUE_ID),
    sql:delete(Sql).

