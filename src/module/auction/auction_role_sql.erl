-module(auction_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("auction.hrl").
-define(INSERT_AUCTION_ROLE, <<"INSERT INTO `auction_role` (`role_id`, `unique_id`, `server_id`, `type`, `time`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(SELECT_AUCTION_ROLE, <<"SELECT * FROM `auction_role`">>).
-define(UPDATE_AUCTION_ROLE, <<"UPDATE `auction_role` SET `server_id` = '~w', `type` = '~w', `time` = '~w' WHERE `role_id` = '~w' AND `unique_id` = '~w'">>).
-define(DELETE_AUCTION_ROLE, <<"DELETE  FROM `auction_role` WHERE `role_id` = '~w' AND `unique_id` = '~w'">>).
-define(INSERT_UPDATE_AUCTION_ROLE, {<<"INSERT INTO `auction_role` (`role_id`, `unique_id`, `server_id`, `type`, `time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `type` = VALUES(`type`), `time` = VALUES(`time`)">>}).

%% @doc insert
insert(AuctionRole) ->
    Sql = parser:format(?INSERT_AUCTION_ROLE, [
        AuctionRole#auction_role.role_id,
        AuctionRole#auction_role.unique_id,
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.time
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_AUCTION_ROLE, []),
    sql:select(Sql).

%% @doc update
update(AuctionRole) ->
    Sql = parser:format(?UPDATE_AUCTION_ROLE, [
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.time,
        AuctionRole#auction_role.role_id,
        AuctionRole#auction_role.unique_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, UniqueId) ->
    Sql = parser:format(?DELETE_AUCTION_ROLE, [RoleId, UniqueId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(AuctionRole) -> [
        AuctionRole#auction_role.role_id,
        AuctionRole#auction_role.unique_id,
        AuctionRole#auction_role.server_id,
        AuctionRole#auction_role.type,
        AuctionRole#auction_role.time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_AUCTION_ROLE, #auction_role.flag),
    sql:insert(Sql),
    NewData.

