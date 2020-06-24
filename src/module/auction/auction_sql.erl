-module(auction_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").
-define(INSERT_AUCTION, <<"INSERT INTO `auction` (`auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES (~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w, ~w, ~w)">>).
-define(SELECT_AUCTION, <<"SELECT `auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `seller_list`, `bidder_list`, `guild_id`, `timer`, `flag` FROM `auction`">>).
-define(UPDATE_AUCTION, <<"UPDATE `auction` SET `auction_id` = ~w, `number` = ~w, `type` = ~w, `bid_type` = ~w, `start_time` = ~w, `end_time` = ~w, `from` = '~s', `bid_number` = ~w, `now_price` = ~w, `next_price` = ~w, `guild_id` = ~w WHERE `auction_no` = ~w">>).
-define(DELETE_AUCTION, <<"DELETE  FROM `auction` WHERE `auction_no` = ~w">>).
-define(INSERT_UPDATE_AUCTION, {<<"INSERT INTO `auction` (`auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `auction_id` = VALUES(`auction_id`), `number` = VALUES(`number`), `type` = VALUES(`type`), `bid_type` = VALUES(`bid_type`), `start_time` = VALUES(`start_time`), `end_time` = VALUES(`end_time`), `from` = VALUES(`from`), `bid_number` = VALUES(`bid_number`), `now_price` = VALUES(`now_price`), `next_price` = VALUES(`next_price`), `guild_id` = VALUES(`guild_id`)">>}).
-define(DELETE_IN_AUCTION_NO, {<<"DELETE  FROM `auction` WHERE `auction_no` in (">>, <<"~w">>, <<")">>}).
-define(TRUNCATE, <<"TRUNCATE TABLE `auction`">>).

%% @doc insert
insert(Auction) ->
    Sql = parser:format(?INSERT_AUCTION, [
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.bid_type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.now_price,
        Auction#auction.next_price,
        Auction#auction.guild_id
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION, []),
    Data = sql:select(Sql),
    F = fun(Auction = #auction{seller_list = SellerList, bidder_list = BidderList}) -> Auction#auction{seller_list = parser:to_term(SellerList), bidder_list = parser:to_term(BidderList)} end,
    parser:convert(Data, auction, F).

%% @doc update
update(Auction) ->
    Sql = parser:format(?UPDATE_AUCTION, [
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.bid_type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.now_price,
        Auction#auction.next_price,
        Auction#auction.guild_id,
        Auction#auction.auction_no
    ]),
    sql:update(Sql).

%% @doc delete
delete(AuctionNo) ->
    Sql = parser:format(?DELETE_AUCTION, [AuctionNo]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Auction) -> [
        Auction#auction.auction_no,
        Auction#auction.auction_id,
        Auction#auction.number,
        Auction#auction.type,
        Auction#auction.bid_type,
        Auction#auction.start_time,
        Auction#auction.end_time,
        Auction#auction.from,
        Auction#auction.bid_number,
        Auction#auction.now_price,
        Auction#auction.next_price,
        Auction#auction.guild_id
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_AUCTION, #auction.flag),
    sql:insert(Sql),
    NewData.

%% @doc delete
delete_in_auction_no(AuctionNoList) ->
    F = fun(AuctionNo) -> [AuctionNo] end,
    Sql = parser:collect(AuctionNoList, F, ?DELETE_IN_AUCTION_NO),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

