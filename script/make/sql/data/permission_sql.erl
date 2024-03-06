-module(permission_sql).
-export([insert/1]).
-export([save/1]).
-export([select_login/1]).
-export([select_chat/1]).
-include("permission.hrl").

%% @doc insert into permission
-spec insert(Permission :: #permission{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Permission) ->
    db:insert(<<"INSERT INTO `permission` (`permission_no`, `role_id`, `type`, `status`, `begin_time`, `end_time`, `time`, `reason`, `remark`) VALUES (:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:)">>, Permission).

%% @doc insert into permission
-spec save(Permission :: #permission{}) -> InsertIdOrAffectedRows :: non_neg_integer().
save(Permission) ->
    db:insert(<<"INSERT IGNORE INTO `permission` (`permission_no`, `role_id`, `type`, `status`, `begin_time`, `end_time`, `time`, `reason`, `remark`) VALUES (:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:)">>, Permission).

%% @doc select from permission
-spec select_login(RoleId :: non_neg_integer()) -> Rows :: [#permission{}].
select_login(RoleId) ->
    Data = db:select(<<"SELECT `permission_no`, `role_id`, `type`, `status`, `begin_time`, `end_time`, `time`, `reason`, `remark` FROM `permission` WHERE `role_id` = (?) AND `type` = ('login') ORDER BY `permission_no` DESC LIMIT 1">>, [RoleId]),
    parser:convert(Data, permission).

%% @doc select from permission
-spec select_chat(RoleId :: non_neg_integer()) -> Rows :: [#permission{}].
select_chat(RoleId) ->
    Data = db:select(<<"SELECT `permission_no`, `role_id`, `type`, `status`, `begin_time`, `end_time`, `time`, `reason`, `remark` FROM `permission` WHERE `role_id` = (?) AND `type` = ('chat') ORDER BY `permission_no` DESC LIMIT 1">>, [RoleId]),
    parser:convert(Data, permission).
