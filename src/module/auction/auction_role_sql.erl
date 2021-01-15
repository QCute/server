-module(auction_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").
-define(INSERT_AUCTION_ROLE, <<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES (~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w~i)">>).
-define(SELECT_AUCTION_ROLE, <<"SELECT `auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`, 0 AS `flag` FROM `auction_role`">>).
-define(UPDATE_AUCTION_ROLE, {<<"UPDATE `auction_role` SET ~i~i`server_id` = ~w, ~i`role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `type` = ~w, `price` = ~w, `time` = ~w~i ">>, <<"WHERE `auction_no` = ~w AND `role_id` = ~w">>}).
-define(DELETE_AUCTION_ROLE, <<"DELETE  FROM `auction_role` WHERE `auction_no` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_AUCTION_ROLE, {<<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `type` = VALUES(`type`), `price` = VALUES(`price`), `time` = VALUES(`time`)">>}).
-define(DELETE_BY_NO, <<"DELETE FROM `auction_role` WHERE `auction_no` = ~w">>).

%% @doc insert
insert(AuctionRole) ->
    Sql = parser:format(?INSERT_AUCTION_ROLE, AuctionRole),
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, auction_role).

%% @doc update
update(AuctionRole) ->
    Sql = <<(parser:format(element(1, ?UPDATE_AUCTION_ROLE), AuctionRole))/binary, (parser:format(element(2, ?UPDATE_AUCTION_ROLE), [AuctionRole#auction_role.auction_no, AuctionRole#auction_role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(AuctionNo, RoleId) ->
    Sql = parser:format(?DELETE_AUCTION_ROLE, [AuctionNo, RoleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_AUCTION_ROLE, #auction_role.flag),
    db:insert(Sql),
    NewData.

%% @doc delete
delete_by_no(AuctionNo) ->
    Sql = parser:format(?DELETE_BY_NO, [AuctionNo]),
    db:delete(Sql).

