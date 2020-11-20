-module(recharge_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("recharge.hrl").
-define(INSERT_RECHARGE, <<"INSERT INTO `recharge` (`recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `server_id`, `account`, `money`, `status`, `time`) VALUES (~w, '~s', '~s', ~w, '~s', ~w, '~s', ~w, ~w, ~w)">>).
-define(SELECT_RECHARGE, <<"SELECT `recharge_no`, `recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `server_id`, `account`, `money`, `status`, `time` FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_RECHARGE, <<"UPDATE `recharge` SET `recharge_id` = ~w, `order_id` = '~s', `channel` = '~s', `role_id` = ~w, `role_name` = '~s', `server_id` = ~w, `account` = '~s', `money` = ~w, `status` = ~w, `time` = ~w WHERE `recharge_no` = ~w">>).
-define(DELETE_RECHARGE, <<"DELETE  FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_STATUS, <<"UPDATE `recharge` SET `status` = ~w WHERE `recharge_no` = ~w">>).
-define(DELETE_IN_RECHARGE_NO, {<<"DELETE  FROM `recharge` WHERE `recharge_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Recharge) ->
    Sql = parser:format(?INSERT_RECHARGE, [
        Recharge#recharge.recharge_id,
        Recharge#recharge.order_id,
        Recharge#recharge.channel,
        Recharge#recharge.role_id,
        Recharge#recharge.role_name,
        Recharge#recharge.server_id,
        Recharge#recharge.account,
        Recharge#recharge.money,
        Recharge#recharge.status,
        Recharge#recharge.time
    ]),
    sql:insert(Sql).

%% @doc select
select(RechargeNo) ->
    Sql = parser:format(?SELECT_RECHARGE, [RechargeNo]),
    Data = sql:select(Sql),
    parser:convert(Data, recharge).

%% @doc update
update(Recharge) ->
    Sql = parser:format(?UPDATE_RECHARGE, [
        Recharge#recharge.recharge_id,
        Recharge#recharge.order_id,
        Recharge#recharge.channel,
        Recharge#recharge.role_id,
        Recharge#recharge.role_name,
        Recharge#recharge.server_id,
        Recharge#recharge.account,
        Recharge#recharge.money,
        Recharge#recharge.status,
        Recharge#recharge.time,
        Recharge#recharge.recharge_no
    ]),
    sql:update(Sql).

%% @doc delete
delete(RechargeNo) ->
    Sql = parser:format(?DELETE_RECHARGE, [RechargeNo]),
    sql:delete(Sql).

%% @doc update
update_status(ThisStatus, RechargeNo) ->
    Sql = parser:format(?UPDATE_STATUS, [ThisStatus, RechargeNo]),
    sql:update(Sql).

%% @doc delete
delete_in_recharge_no(RechargeNoList) ->
    F = fun(RechargeNo) -> [RechargeNo] end,
    Sql = parser:collect(RechargeNoList, F, ?DELETE_IN_RECHARGE_NO),
    sql:delete(Sql).

