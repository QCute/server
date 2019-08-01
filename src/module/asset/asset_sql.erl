-module(asset_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("asset.hrl").

-define(INSERT_ASSET, <<"INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `exp`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(UPDATE_ASSET, <<"UPDATE `asset` SET `gold` = '~w', `silver` = '~w', `copper` = '~w', `exp` = '~w' WHERE `role_id` = '~w'">>).
-define(SELECT_ASSET, <<"SELECT * FROM `asset` WHERE `role_id` = '~w'">>).
-define(DELETE_ASSET, <<"DELETE  FROM `asset` WHERE `role_id` = '~w'">>).

%% @doc insert
insert(Asset) ->
    Sql = parser:format(?INSERT_ASSET, [
        Asset#asset.role_id,
        Asset#asset.gold,
        Asset#asset.silver,
        Asset#asset.copper,
        Asset#asset.exp
    ]),
    sql:insert(Sql).

%% @doc update
update(Asset) ->
    Sql = parser:format(?UPDATE_ASSET, [
        Asset#asset.gold,
        Asset#asset.silver,
        Asset#asset.copper,
        Asset#asset.exp,
        Asset#asset.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ASSET, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ASSET, [
        RoleId
    ]),
    sql:delete(Sql).

