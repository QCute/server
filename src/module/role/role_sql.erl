-module(role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").
-define(INSERT_ROLE, <<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `level`, `sex`, `classes`, `type`, `status`, `is_online`, `register_time`, `login_time`, `logout_time`, `first_recharge_time`, `last_recharge_time`, `recharge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip`) VALUES (~i~i'~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~s', '~s', '~s')">>).
-define(SELECT_ROLE, <<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `level`, `sex`, `classes`, `type`, `status`, `is_online`, `register_time`, `login_time`, `logout_time`, `first_recharge_time`, `last_recharge_time`, `recharge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip` FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_ROLE, {<<"UPDATE `role` SET ~i~i`role_name` = '~s', `server_id` = ~w, `account_name` = '~s', `origin_server_id` = ~w, `level` = ~w, `sex` = ~w, `classes` = ~w, `type` = ~w, `status` = ~w, `is_online` = ~w, `register_time` = ~w, `login_time` = ~w, `logout_time` = ~w, `first_recharge_time` = ~w, `last_recharge_time` = ~w, `recharge_total` = ~w, `item_size` = ~w, `bag_size` = ~w, `store_size` = ~w, `map` = '~w', `channel` = '~s', `device_id` = '~s', `device_type` = '~s', `mac` = '~s', `ip` = '~s' ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_ROLE, <<"DELETE  FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_NAME, <<"UPDATE `role` SET `role_name` = '~s' WHERE `role_id` = ~w">>).
-define(DELETE_IN_ROLE_ID, {<<"DELETE  FROM `role` WHERE `role_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Role) ->
    Sql = parser:format(?INSERT_ROLE, Role),
    db:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ROLE, [RoleId]),
    Data = db:select(Sql),
    F = fun(Role = #role{map = Map}) -> Role#role{map = parser:to_term(Map)} end,
    parser:convert(Data, role, F).

%% @doc update
update(Role) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ROLE), Role))/binary, (parser:format(element(2, ?UPDATE_ROLE), [Role#role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ROLE, [RoleId]),
    db:delete(Sql).

%% @doc update
update_name(ThisRoleName, RoleId) ->
    Sql = parser:format(?UPDATE_NAME, [ThisRoleName, RoleId]),
    db:update(Sql).

%% @doc delete
delete_in_role_id(RoleIdList) ->
    Sql = parser:collect(RoleIdList, ?DELETE_IN_ROLE_ID),
    db:delete(Sql).

