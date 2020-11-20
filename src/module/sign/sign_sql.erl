-module(sign_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("sign.hrl").
-define(INSERT_SIGN, <<"INSERT INTO `sign` (`role_id`, `login_day`, `sign_total`, `is_sign_today`) VALUES (~w, ~w, ~w, ~w)">>).
-define(SELECT_SIGN, <<"SELECT `role_id`, `login_day`, `sign_total`, `is_sign_today` FROM `sign` WHERE `role_id` = ~w">>).
-define(UPDATE_SIGN, <<"UPDATE `sign` SET `login_day` = ~w, `sign_total` = ~w, `is_sign_today` = ~w WHERE `role_id` = ~w">>).
-define(DELETE_SIGN, <<"DELETE  FROM `sign` WHERE `role_id` = ~w">>).

%% @doc insert
insert(Sign) ->
    Sql = parser:format(?INSERT_SIGN, [
        Sign#sign.role_id,
        Sign#sign.login_day,
        Sign#sign.sign_total,
        Sign#sign.is_sign_today
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_SIGN, [RoleId]),
    Data = sql:select(Sql),
    parser:convert(Data, sign).

%% @doc update
update(Sign) ->
    Sql = parser:format(?UPDATE_SIGN, [
        Sign#sign.login_day,
        Sign#sign.sign_total,
        Sign#sign.is_sign_today,
        Sign#sign.role_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_SIGN, [RoleId]),
    sql:delete(Sql).

