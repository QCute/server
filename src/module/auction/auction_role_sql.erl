-module(auction_role_sql).
-export([save/1]).
-export([select/0]).
-export([delete_by_auction_no/1]).
-include("auction.hrl").

%% @doc insert into auction_role
-spec save(AuctionRoleList :: [#auction_role{}] | ets:tab()) -> NewAuctionRoleList :: [#auction_role{}].
save(AuctionRoleList) ->
    db:save(<<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, AuctionRoleList, fun(#auction_role{auction_no = AuctionNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, type = Type, price = Price, time = Time}) -> [AuctionNo, ServerId, RoleId, RoleName, GuildId, GuildName, Type, Price, Time] end, #auction_role.flag).

%% @doc select from auction_role
-spec select() -> Rows :: [#auction_role{}].
select() ->
    Data = db:select(<<"SELECT `auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time` FROM `auction_role`">>, []),
    parser:convert(Data, auction_role).

%% @doc delete row from auction_role
-spec delete_by_auction_no(AuctionNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_auction_no(AuctionNo) ->
    db:delete(<<"DELETE FROM `auction_role` WHERE `auction_no` = ?">>, [AuctionNo]).
