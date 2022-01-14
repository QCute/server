-module(auction_role_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([delete_by_no/1]).
-include("auction.hrl").

-define(INSERT_AUCTION_ROLE, <<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES (~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w~i)">>).
-define(SELECT_AUCTION_ROLE, <<"SELECT `auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`, 0 AS `flag` FROM `auction_role`">>).
-define(UPDATE_AUCTION_ROLE, {<<"UPDATE `auction_role` SET ~i~i`server_id` = ~w, ~i`role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `type` = ~w, `price` = ~w, `time` = ~w~i ">>, <<"WHERE `auction_no` = ~w AND `role_id` = ~w">>}).
-define(DELETE_AUCTION_ROLE, <<"DELETE FROM `auction_role` WHERE `auction_no` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_AUCTION_ROLE, {<<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `type` = VALUES(`type`), `price` = VALUES(`price`), `time` = VALUES(`time`)">>}).
-define(DELETE_BY_NO, <<"DELETE FROM `auction_role` WHERE `auction_no` = ~w">>).

%% @doc insert
-spec insert(AuctionRole :: #auction_role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(AuctionRole) ->
    Sql = parser:format(?INSERT_AUCTION_ROLE, AuctionRole),
    db:insert(Sql).

%% @doc select
-spec select() -> AuctionRoleList :: [#auction_role{}].
select() ->
    Sql = parser:format(?SELECT_AUCTION_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, auction_role).

%% @doc update
-spec update(AuctionRole :: #auction_role{}) -> AffectedRows :: non_neg_integer().
update(AuctionRole) ->
    Sql = <<(parser:format(element(1, ?UPDATE_AUCTION_ROLE), AuctionRole))/binary, (parser:format(element(2, ?UPDATE_AUCTION_ROLE), [AuctionRole#auction_role.auction_no, AuctionRole#auction_role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(AuctionNo :: integer(), RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(AuctionNo, RoleId) ->
    Sql = parser:format(?DELETE_AUCTION_ROLE, [AuctionNo, RoleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(AuctionRoleList :: [#auction_role{}] | ets:tab()) -> NewAuctionRoleList :: [#auction_role{}].
insert_update(AuctionRoleList) ->
    {Sql, NewAuctionRoleList} = parser:collect_into(AuctionRoleList, ?INSERT_UPDATE_AUCTION_ROLE, #auction_role.flag),
    db:insert(Sql),
    NewAuctionRoleList.

%% @doc delete
-spec delete_by_no(AuctionNo :: integer()) -> AffectedRows :: non_neg_integer().
delete_by_no(AuctionNo) ->
    Sql = parser:format(?DELETE_BY_NO, [AuctionNo]),
    db:delete(Sql).

