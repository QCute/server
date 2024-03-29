-module(auction_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/1]).
-export([insert_update/1]).
-export([delete_in_auction_no/1]).
-include("auction.hrl").

-define(INSERT_AUCTION, <<"INSERT INTO `auction` (`auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES (~i~i~w, ~w, ~w, ~w, ~w, ~w, '~w', ~w, ~w, ~w, ~i~i~w~i~i)">>).
-define(SELECT_AUCTION, <<"SELECT `auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, '' AS `seller_list`, '' AS `bidder_list`, `guild_id`, 0 AS `timer`, 0 AS `flag` FROM `auction`">>).
-define(UPDATE_AUCTION, {<<"UPDATE `auction` SET ~i~i`auction_id` = ~w, `number` = ~w, `type` = ~w, `bid_type` = ~w, `start_time` = ~w, `end_time` = ~w, `from` = '~w', `bid_number` = ~w, `now_price` = ~w, `next_price` = ~w, ~i~i`guild_id` = ~w~i~i ">>, <<"WHERE `auction_no` = ~w">>}).
-define(DELETE_AUCTION, <<"DELETE FROM `auction` WHERE `auction_no` = ~w">>).
-define(INSERT_UPDATE_AUCTION, {<<"INSERT INTO `auction` (`auction_no`, `auction_id`, `number`, `type`, `bid_type`, `start_time`, `end_time`, `from`, `bid_number`, `now_price`, `next_price`, `guild_id`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', ~w, ~w, ~w, ~i~i~w~i~i)">>, <<" ON DUPLICATE KEY UPDATE `auction_id` = VALUES(`auction_id`), `number` = VALUES(`number`), `type` = VALUES(`type`), `bid_type` = VALUES(`bid_type`), `start_time` = VALUES(`start_time`), `end_time` = VALUES(`end_time`), `from` = VALUES(`from`), `bid_number` = VALUES(`bid_number`), `now_price` = VALUES(`now_price`), `next_price` = VALUES(`next_price`), `guild_id` = VALUES(`guild_id`)">>}).
-define(DELETE_IN_AUCTION_NO, {<<"DELETE FROM `auction` WHERE `auction_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Auction :: #auction{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Auction) ->
    Sql = parser:format(?INSERT_AUCTION, Auction),
    db:insert(Sql).

%% @doc select
-spec select() -> AuctionList :: [#auction{}].
select() ->
    Sql = parser:format(?SELECT_AUCTION, []),
    Data = db:select(Sql),
    F = fun(Auction = #auction{from = From}) -> Auction#auction{from = parser:to_term(From)} end,
    parser:convert(Data, auction, F).

%% @doc update
-spec update(Auction :: #auction{}) -> AffectedRows :: non_neg_integer().
update(Auction) ->
    Sql = <<(parser:format(element(1, ?UPDATE_AUCTION), Auction))/binary, (parser:format(element(2, ?UPDATE_AUCTION), [Auction#auction.auction_no]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(AuctionNo :: integer()) -> AffectedRows :: non_neg_integer().
delete(AuctionNo) ->
    Sql = parser:format(?DELETE_AUCTION, [AuctionNo]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(AuctionList :: [#auction{}] | ets:tab()) -> NewAuctionList :: [#auction{}].
insert_update(AuctionList) ->
    {Sql, NewAuctionList} = parser:collect_into(AuctionList, ?INSERT_UPDATE_AUCTION, #auction.flag),
    db:insert(Sql),
    NewAuctionList.

%% @doc delete
-spec delete_in_auction_no(AuctionNoList :: [AuctionNo :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_auction_no(AuctionNoList) ->
    Sql = parser:collect(AuctionNoList, ?DELETE_IN_AUCTION_NO),
    db:delete(Sql).

