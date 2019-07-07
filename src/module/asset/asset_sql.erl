-module(asset_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("asset.hrl").

-define(INSERT_asset, "INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `exp`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_asset, "UPDATE `asset` SET `gold` = '~w', `silver` = '~w', `copper` = '~w', `exp` = '~w' WHERE `role_id` = '~w'").
-define(SELECT_asset, "SELECT * FROM `asset` WHERE `role_id` = '~w'").
-define(DELETE_asset, "DELETE  FROM `asset` WHERE `role_id` = '~w'").

%% @doc insert
insert(Asset) ->
    Sql = io_lib:format(?INSERT_asset, [
        Asset#asset.role_id,
        Asset#asset.gold,
        Asset#asset.silver,
        Asset#asset.copper,
        Asset#asset.exp
    ]),
    sql:insert(Sql).

%% @doc update
update(Asset) ->
    Sql = io_lib:format(?UPDATE_asset, [
        Asset#asset.gold,
        Asset#asset.silver,
        Asset#asset.copper,
        Asset#asset.exp,
        Asset#asset.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_asset, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = io_lib:format(?DELETE_asset, [
        RoleId
    ]),
    sql:delete(Sql).

