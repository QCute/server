-module(auction_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").
-define(INSERT_AUCTION_ROLE, <<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES (~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w)">>).
-define(SELECT_AUCTION_ROLE, <<"SELECT `auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`, 0 AS `flag` FROM `auction_role`">>).
-define(UPDATE_AUCTION_ROLE, <<"UPDATE `auction_role` SET `server_id` = ~w, `role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `type` = ~w, `price` = ~w, `time` = ~w WHERE `auction_no` = ~w AND `role_id` = ~w">>).
-define(DELETE_AUCTION_ROLE, <<"DELETE  FROM `auction_role` WHERE `auction_no` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_AUCTION_ROLE, {<<"INSERT INTO `auction_role` (`auction_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `type`, `price`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `type` = VALUES(`type`), `price` = VALUES(`price`), `time` = VALUES(`time`)">>}).
-define(DELETE_BY_NO, <<"DELETE FROM `auction_role` WHERE `auction_no` = ~w">>).
-define(TRUNCATE, <<"TRUNCATE TABLE `auction_role`">>).

%% @doc insert
insert(AuctionRole) ->
    Sql = parser:format(?INSERT_AUCTION_ROLE, [
        AuctionRole#auction_role.auction_no,
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.role_id,
        AuctionRole#auction_role.role_name,
        AuctionRole#auction_role.guild_id,
        AuctionRole#auction_role.guild_name,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.price,
        AuctionRole#auction_role.time
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION_ROLE, []),
    Data = sql:select(Sql),
    parser:convert(Data, auction_role).

%% @doc update
update(AuctionRole) ->
    Sql = parser:format(?UPDATE_AUCTION_ROLE, [
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.role_name,
        AuctionRole#auction_role.guild_id,
        AuctionRole#auction_role.guild_name,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.price,
        AuctionRole#auction_role.time,
        AuctionRole#auction_role.auction_no,
        AuctionRole#auction_role.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(AuctionNo, RoleId) ->
    Sql = parser:format(?DELETE_AUCTION_ROLE, [AuctionNo, RoleId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(AuctionRole) -> [
        AuctionRole#auction_role.auction_no,
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.role_id,
        AuctionRole#auction_role.role_name,
        AuctionRole#auction_role.guild_id,
        AuctionRole#auction_role.guild_name,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.price,
        AuctionRole#auction_role.time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_AUCTION_ROLE, #auction_role.flag),
    sql:insert(Sql),
    NewData.

%% @doc delete
delete_by_no(AuctionNo) ->
    Sql = parser:format(?DELETE_BY_NO, [AuctionNo]),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

