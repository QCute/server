-module(account_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("account.hrl").

-define(INSERT_ACCOUNT, "INSERT INTO `account` (`role_id`, `agent_id`, `device`, `device_type`, `mac`) VALUES ('~w', '~w', '~s', '~s', '~s')").
-define(UPDATE_ACCOUNT, "UPDATE `account` SET `agent_id` = '~w', `device` = '~s', `device_type` = '~s', `mac` = '~s' WHERE `role_id` = '~w'").
-define(SELECT_ACCOUNT, "SELECT * FROM `account` WHERE `role_id` = '~w'").
-define(DELETE_ACCOUNT, "DELETE  FROM `account` WHERE `role_id` = '~w'").

%% @doc insert
insert(Account) ->
    Sql = io_lib:format(?INSERT_ACCOUNT, [
        Account#account.role_id,
        Account#account.agent_id,
        Account#account.device,
        Account#account.device_type,
        Account#account.mac
    ]),
    sql:insert(Sql).

%% @doc update
update(Account) ->
    Sql = io_lib:format(?UPDATE_ACCOUNT, [
        Account#account.agent_id,
        Account#account.device,
        Account#account.device_type,
        Account#account.mac,
        Account#account.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = io_lib:format(?SELECT_ACCOUNT, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = io_lib:format(?DELETE_ACCOUNT, [
        RoleId
    ]),
    sql:delete(Sql).

