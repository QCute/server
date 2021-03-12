-module(recharge_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("recharge.hrl").
-define(INSERT_RECHARGE, <<"INSERT INTO `recharge` (`recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (~i~i~w, '~s', '~s', ~w, '~s', ~w, ~w, ~w)">>).
-define(SELECT_RECHARGE, <<"SELECT `recharge_no`, `recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_RECHARGE, {<<"UPDATE `recharge` SET ~i~i`recharge_id` = ~w, `order_id` = '~s', `channel` = '~s', `role_id` = ~w, `role_name` = '~s', `money` = ~w, `status` = ~w, `time` = ~w ">>, <<"WHERE `recharge_no` = ~w">>}).
-define(DELETE_RECHARGE, <<"DELETE  FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_STATUS, <<"UPDATE `recharge` SET `status` = ~w WHERE `recharge_no` = ~w">>).
-define(DELETE_IN_RECHARGE_NO, {<<"DELETE  FROM `recharge` WHERE `recharge_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Recharge) ->
    Sql = parser:format(?INSERT_RECHARGE, Recharge),
    db:insert(Sql).

%% @doc select
select(RechargeNo) ->
    Sql = parser:format(?SELECT_RECHARGE, [RechargeNo]),
    Data = db:select(Sql),
    parser:convert(Data, recharge).

%% @doc update
update(Recharge) ->
    Sql = <<(parser:format(element(1, ?UPDATE_RECHARGE), Recharge))/binary, (parser:format(element(2, ?UPDATE_RECHARGE), [Recharge#recharge.recharge_no]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RechargeNo) ->
    Sql = parser:format(?DELETE_RECHARGE, [RechargeNo]),
    db:delete(Sql).

%% @doc update
update_status(ThisStatus, RechargeNo) ->
    Sql = parser:format(?UPDATE_STATUS, [ThisStatus, RechargeNo]),
    db:update(Sql).

%% @doc delete
delete_in_recharge_no(RechargeNoList) ->
    Sql = parser:collect(RechargeNoList, ?DELETE_IN_RECHARGE_NO),
    db:delete(Sql).

