-module(role_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-export([update_name/2]).
-export([delete_in_role_id/1]).
-include("role.hrl").

-define(INSERT_ROLE, <<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `online_time`, `logout_time`, `world_chat_time`, `guild_chat_time`, `first_charge_time`, `last_charge_time`, `charge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip`) VALUES (~i~i'~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~s', '~s', '~s')">>).
-define(SELECT_ROLE, <<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `online_time`, `logout_time`, `world_chat_time`, `guild_chat_time`, `first_charge_time`, `last_charge_time`, `charge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip` FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_ROLE, {<<"UPDATE `role` SET ~i~i`role_name` = '~s', `server_id` = ~w, `account_name` = '~s', `origin_server_id` = ~w, `type` = ~w, `status` = ~w, `sex` = ~w, `avatar` = ~w, `classes` = ~w, `level` = ~w, `is_online` = ~w, `register_time` = ~w, `login_time` = ~w, `online_time` = ~w, `logout_time` = ~w, `world_chat_time` = ~w, `guild_chat_time` = ~w, `first_charge_time` = ~w, `last_charge_time` = ~w, `charge_total` = ~w, `item_size` = ~w, `bag_size` = ~w, `store_size` = ~w, `map` = '~w', `channel` = '~s', `device_id` = '~s', `device_type` = '~s', `mac` = '~s', `ip` = '~s' ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_ROLE, <<"DELETE FROM `role` WHERE `role_id` = ~w">>).
-define(UPDATE_NAME, <<"UPDATE `role` SET `role_name` = '~s' WHERE `role_id` = ~w">>).
-define(DELETE_IN_ROLE_ID, {<<"DELETE FROM `role` WHERE `role_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Role :: #role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Role) ->
    Sql = parser:format(?INSERT_ROLE, Role),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer()) -> RoleList :: [#role{}].
select(RoleId) ->
    Sql = parser:format(?SELECT_ROLE, [RoleId]),
    Data = db:select(Sql),
    F = fun(Role = #role{map = Map}) -> Role#role{map = parser:to_term(Map)} end,
    parser:convert(Data, role, F).

%% @doc update
-spec update(Role :: #role{}) -> AffectedRows :: non_neg_integer().
update(Role) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ROLE), Role))/binary, (parser:format(element(2, ?UPDATE_ROLE), [Role#role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId) ->
    Sql = parser:format(?DELETE_ROLE, [RoleId]),
    db:delete(Sql).

%% @doc update
-spec update_name(UpdateRoleName :: binary(), RoleId :: integer()) -> non_neg_integer().
update_name(UpdateRoleName, RoleId) ->
    Sql = parser:format(?UPDATE_NAME, [UpdateRoleName, RoleId]),
    db:update(Sql).

%% @doc delete
-spec delete_in_role_id(RoleIdList :: [RoleId :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_role_id(RoleIdList) ->
    Sql = parser:collect(RoleIdList, ?DELETE_IN_ROLE_ID),
    db:delete(Sql).

