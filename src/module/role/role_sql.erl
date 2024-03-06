-module(role_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([update_name/1]).
-include("role.hrl").

%% @doc insert into role
-spec insert(Role :: #role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Role) ->
    db:insert(<<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time`) VALUES (:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:, :12:, :13:, :14:, :15:, :16:)">>, Role).

%% @doc select from role
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#role{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time` FROM `role` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, role).

%% @doc update into role
-spec update(Role :: #role{}) -> AffectedRows :: non_neg_integer().
update(Role) ->
    db:update(<<"UPDATE `role` SET `role_name` = :2:, `server_id` = :3:, `account_name` = :4:, `origin_server_id` = :5:, `channel` = :6:, `type` = :7:, `status` = :8:, `sex` = :9:, `avatar` = :10:, `classes` = :11:, `level` = :12:, `is_online` = :13:, `register_time` = :14:, `login_time` = :15:, `logout_time` = :16: WHERE `role_id` = :1:">>, Role).

%% @doc update into role
-spec update_name(Role :: #role{}) -> AffectedRows :: non_neg_integer().
update_name(Role) ->
    db:update(<<"UPDATE `role` SET `role_name` = :2: WHERE `role_id` = :1:">>, Role).
