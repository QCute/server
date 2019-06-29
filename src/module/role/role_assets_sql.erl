-module(role_assets_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("assets.hrl").

-define(INSERT_ASSETS, "INSERT INTO `assets` (`role_id`, `gold`, `silver`, `copper`, `exp`) VALUES ('~w', '~w', '~w', '~w', '~w')").
-define(UPDATE_ASSETS, "UPDATE `assets` SET `gold` = '~w', `silver` = '~w', `copper` = '~w', `exp` = '~w' WHERE `role_id` = '~w'").
-define(SELECT_ASSETS, "SELECT * FROM `assets` WHERE `role_id` = '~w'").
-define(DELETE_ASSETS, "DELETE  FROM `assets` WHERE `role_id` = '~w'").

%% @doc insert
insert(Assets) ->
    Sql = io_lib:format(?INSERT_ASSETS, [
        Assets#assets.role_id,
        Assets#assets.gold,
        Assets#assets.silver,
        Assets#assets.copper,
        Assets#assets.exp
    ]),
    sql:insert(Sql).

%% @doc update
update(Assets) ->
    Sql = io_lib:format(?UPDATE_ASSETS, [
        Assets#assets.gold,
        Assets#assets.silver,
        Assets#assets.copper,
        Assets#assets.exp,
        Assets#assets.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_ASSETS, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = io_lib:format(?DELETE_ASSETS, [
        RoleId
    ]),
    sql:delete(Sql).

