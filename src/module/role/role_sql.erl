-module(role_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([update_name/1]).
-include("role.hrl").

%% @doc insert into role
-spec insert(Role :: #role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#role{role_name = RoleName, server_id = ServerId, account_name = AccountName, origin_server_id = OriginServerId, channel = Channel, type = Type, status = Status, sex = Sex, avatar = Avatar, classes = Classes, level = Level, is_online = IsOnline, register_time = RegisterTime, login_time = LoginTime, logout_time = LogoutTime}) ->
    db:insert(<<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, [RoleName, ServerId, AccountName, OriginServerId, Channel, Type, Status, Sex, Avatar, Classes, Level, IsOnline, RegisterTime, LoginTime, LogoutTime]).

%% @doc select from role
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#role{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `channel`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time` FROM `role` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, role).

%% @doc update into role
-spec update(#role{}) -> AffectedRows :: non_neg_integer().
update(#role{role_name = RoleName, server_id = ServerId, account_name = AccountName, origin_server_id = OriginServerId, channel = Channel, type = Type, status = Status, sex = Sex, avatar = Avatar, classes = Classes, level = Level, is_online = IsOnline, register_time = RegisterTime, login_time = LoginTime, logout_time = LogoutTime, role_id = RoleId}) ->
    db:update(<<"UPDATE `role` SET `role_name` = ?, `server_id` = ?, `account_name` = ?, `origin_server_id` = ?, `channel` = ?, `type` = ?, `status` = ?, `sex` = ?, `avatar` = ?, `classes` = ?, `level` = ?, `is_online` = ?, `register_time` = ?, `login_time` = ?, `logout_time` = ? WHERE `role_id` = ?">>, [RoleName, ServerId, AccountName, OriginServerId, Channel, Type, Status, Sex, Avatar, Classes, Level, IsOnline, RegisterTime, LoginTime, LogoutTime, RoleId]).

%% @doc update into role
-spec update_name(#role{}) -> AffectedRows :: non_neg_integer().
update_name(#role{role_name = RoleName, role_id = RoleId}) ->
    db:update(<<"UPDATE `role` SET `role_name` = ? WHERE `role_id` = ?">>, [RoleName, RoleId]).
