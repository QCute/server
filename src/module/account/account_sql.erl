-module(account_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("account.hrl").

-define(INSERT_ACCOUNT, <<"INSERT INTO `account` (`role_id`, `agent_id`, `device`, `device_type`, `mac`) VALUES ('~w', '~w', '~w', '~w', '~w')">>).
-define(UPDATE_ACCOUNT, <<"UPDATE `account` SET `agent_id` = '~w', `device` = '~w', `device_type` = '~w', `mac` = '~w' WHERE `role_id` = '~w'">>).
-define(SELECT_ACCOUNT, <<"SELECT * FROM `account` WHERE `role_id` = '~w'">>).
-define(DELETE_ACCOUNT, <<"DELETE  FROM `account` WHERE `role_id` = '~w'">>).

%% @doc insert
insert(Account) ->
    Sql = parser:format(?INSERT_ACCOUNT, [
        Account#account.role_id,
        Account#account.agent_id,
        Account#account.device,
        Account#account.device_type,
        Account#account.mac
    ]),
    sql:insert(Sql).

%% @doc update
update(Account) ->
    Sql = parser:format(?UPDATE_ACCOUNT, [
        Account#account.agent_id,
        Account#account.device,
        Account#account.device_type,
        Account#account.mac,
        Account#account.role_id
    ]),
    sql:update(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ACCOUNT, [
        RoleId
    ]),
    sql:select(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ACCOUNT, [
        RoleId
    ]),
    sql:delete(Sql).

