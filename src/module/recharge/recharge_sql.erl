-module(recharge_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-export([update_status/2]).
-export([delete_in_recharge_no/1]).
-include("recharge.hrl").

-define(INSERT_RECHARGE, <<"INSERT INTO `recharge` (`recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (~i~i~w, '~s', '~s', ~w, '~s', ~w, ~w, ~w)">>).
-define(SELECT_RECHARGE, <<"SELECT `recharge_no`, `recharge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_RECHARGE, {<<"UPDATE `recharge` SET ~i~i`recharge_id` = ~w, `order_id` = '~s', `channel` = '~s', `role_id` = ~w, `role_name` = '~s', `money` = ~w, `status` = ~w, `time` = ~w ">>, <<"WHERE `recharge_no` = ~w">>}).
-define(DELETE_RECHARGE, <<"DELETE FROM `recharge` WHERE `recharge_no` = ~w">>).
-define(UPDATE_STATUS, <<"UPDATE `recharge` SET `status` = ~w WHERE `recharge_no` = ~w">>).
-define(DELETE_IN_RECHARGE_NO, {<<"DELETE FROM `recharge` WHERE `recharge_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Recharge :: #recharge{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Recharge) ->
    Sql = parser:format(?INSERT_RECHARGE, Recharge),
    db:insert(Sql).

%% @doc select
-spec select(RechargeNo :: integer()) -> RechargeList :: [#recharge{}].
select(RechargeNo) ->
    Sql = parser:format(?SELECT_RECHARGE, [RechargeNo]),
    Data = db:select(Sql),
    parser:convert(Data, recharge).

%% @doc update
-spec update(Recharge :: #recharge{}) -> AffectedRows :: non_neg_integer().
update(Recharge) ->
    Sql = <<(parser:format(element(1, ?UPDATE_RECHARGE), Recharge))/binary, (parser:format(element(2, ?UPDATE_RECHARGE), [Recharge#recharge.recharge_no]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RechargeNo :: integer()) -> AffectedRows :: non_neg_integer().
delete(RechargeNo) ->
    Sql = parser:format(?DELETE_RECHARGE, [RechargeNo]),
    db:delete(Sql).

%% @doc update
-spec update_status(UpdateStatus :: integer(), RechargeNo :: integer()) -> non_neg_integer().
update_status(UpdateStatus, RechargeNo) ->
    Sql = parser:format(?UPDATE_STATUS, [UpdateStatus, RechargeNo]),
    db:update(Sql).

%% @doc delete
-spec delete_in_recharge_no(RechargeNoList :: [RechargeNo :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_recharge_no(RechargeNoList) ->
    Sql = parser:collect(RechargeNoList, ?DELETE_IN_RECHARGE_NO),
    db:delete(Sql).

