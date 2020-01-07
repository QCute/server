-module(role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").
-define(INSERT_ROLE, <<"INSERT INTO `role` (`role_name`, `account`, `type`, `level`, `sex`, `classes`, `item_size`, `bag_size`, `store_size`, `online`, `online_time`, `server_id`, `channel_id`, `map`, `device_id`, `device_type`, `mac`) VALUES ('~s', '~s', ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~s')">>).
-define(SELECT_ROLE, <<"SELECT `role_id`, `role_name`, `account`, `type`, `level`, `sex`, `classes`, `item_size`, `bag_size`, `store_size`, `online`, `online_time`, `server_id`, `channel_id`, `map`, `device_id`, `device_type`, `mac` FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_ROLE, <<"UPDATE `role` SET `type` = ~w, `level` = ~w, `sex` = ~w, `classes` = ~w, `item_size` = ~w, `bag_size` = ~w, `store_size` = ~w, `online` = ~w, `online_time` = ~w, `server_id` = ~w, `channel_id` = ~w, `map` = '~w', `device_id` = '~s', `device_type` = '~s', `mac` = '~s' WHERE `role_id` = ~w">>).
-define(DELETE_ROLE, <<"DELETE  FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_NAME, <<"UPDATE `role` SET `role_name` = '~s' WHERE `role_id` = ~w">>).
-define(DELETE_IN_ROLE_ID, {<<"DELETE  FROM `role` WHERE `role_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Role) ->
    Sql = parser:format(?INSERT_ROLE, [
        Role#role.role_name,
        Role#role.account,
        Role#role.type,
        Role#role.level,
        Role#role.sex,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.online,
        Role#role.online_time,
        Role#role.server_id,
        Role#role.channel_id,
        Role#role.map,
        Role#role.device_id,
        Role#role.device_type,
        Role#role.mac
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ROLE, [RoleId]),
    Data = sql:select(Sql),
    F = fun(Role = #role{map = Map}) -> Role#role{map = parser:to_term(Map)} end,
    parser:convert(Data, role, F).

%% @doc update
update(Role) ->
    Sql = parser:format(?UPDATE_ROLE, [
        Role#role.type,
        Role#role.level,
        Role#role.sex,
        Role#role.classes,
        Role#role.item_size,
        Role#role.bag_size,
        Role#role.store_size,
        Role#role.online,
        Role#role.online_time,
        Role#role.server_id,
        Role#role.channel_id,
        Role#role.map,
        Role#role.device_id,
        Role#role.device_type,
        Role#role.mac,
        Role#role.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ROLE, [RoleId]),
    sql:delete(Sql).

%% @doc update
update_name(RoleName, RoleId) ->
    Sql = parser:format(?UPDATE_NAME, [RoleName, RoleId]),
    sql:update(Sql).

%% @doc delete
delete_in_role_id(RoleIdList) ->
    F = fun(RoleId) -> [RoleId] end,
    Sql = parser:collect(RoleIdList, F, ?DELETE_IN_ROLE_ID),
    sql:delete(Sql).

