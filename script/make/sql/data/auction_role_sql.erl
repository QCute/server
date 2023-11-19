-module(auction_role_sql).
-export([save/1]).
-export([select/0]).
-export([delete_by_auction_no/1]).
-include("auction.hrl").

%% @doc insert into auction_role
-spec save(AuctionRoleList :: [#auction_role{}] | ets:tab()) -> NewAuctionRoleList :: [#auction_role{}].
save(AuctionRoleList) ->
    db:save_into(<<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:)">>, <<"ON DUPLICATE KEY UPDATE `auction_no` = VALUES(`auction_no`), `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `type` = VALUES(`type`), `price` = VALUES(`price`), `time` = VALUES(`time`)">>, AuctionRoleList, #auction_role.flag).

%% @doc select from auction_role
-spec select() -> Rows :: [#auction_role{}].
select() ->
    Data = db:select(<<"SELECT `auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`, `flag` FROM `auction_role`">>, []),
    parser:convert(Data, auction_role).

%% @doc delete row from auction_role
-spec delete_by_auction_no(AuctionNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_auction_no(AuctionNo) ->
    db:delete(<<"DELETE FROM `auction_role` WHERE `auction_no` = ?">>, [AuctionNo]).
