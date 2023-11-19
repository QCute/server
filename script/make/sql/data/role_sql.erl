-module(role_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([update_name/1]).
-include("role.hrl").

%% @doc insert into role
-spec insert(Role :: #role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Role) ->
    db:insert(<<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time`) VALUES (:3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:, :12:, :13:, :14:, :15:, :16:, :17:)">>, Role).

%% @doc select from role
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#role{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time` FROM `role` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, role).

%% @doc update into role
-spec update(Role :: #role{}) -> AffectedRows :: non_neg_integer().
update(Role) ->
    db:update(<<"UPDATE `role` SET `role_name` = :3:, `server_id` = :4:, `account_name` = :5:, `origin_server_id` = :6:, `channel` = :7:, `type` = :8:, `status` = :9:, `sex` = :10:, `avatar` = :11:, `classes` = :12:, `level` = :13:, `is_online` = :14:, `register_time` = :15:, `login_time` = :16:, `logout_time` = :17: WHERE `role_id` = :1:">>, Role).

%% @doc update into role
-spec update_name(Role :: #role{}) -> AffectedRows :: non_neg_integer().
update_name(Role) ->
    db:update(<<"UPDATE `role` SET `role_name` = :3: WHERE `role_id` = :1:">>, Role).
