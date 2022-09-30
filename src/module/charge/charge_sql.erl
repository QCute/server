-module(charge_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-export([update_status/2]).
-export([delete_in_charge_no/1]).
-include("charge.hrl").

-define(INSERT_CHARGE, <<"INSERT INTO `charge` (`charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (~i~i~w, '~s', '~s', ~w, '~s', ~w, ~w, ~w)">>).
-define(SELECT_CHARGE, <<"SELECT `charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `charge` WHERE `charge_no` = ~w">>).
-define(UPDATE_CHARGE, {<<"UPDATE `charge` SET ~i~i`charge_id` = ~w, `order_id` = '~s', `channel` = '~s', `role_id` = ~w, `role_name` = '~s', `money` = ~w, `status` = ~w, `time` = ~w ">>, <<"WHERE `charge_no` = ~w">>}).
-define(DELETE_CHARGE, <<"DELETE FROM `charge` WHERE `charge_no` = ~w">>).
-define(UPDATE_STATUS, <<"UPDATE `charge` SET `status` = ~w WHERE `charge_no` = ~w">>).
-define(DELETE_IN_CHARGE_NO, {<<"DELETE FROM `charge` WHERE `charge_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Charge :: #charge{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Charge) ->
    Sql = parser:format(?INSERT_CHARGE, Charge),
    db:insert(Sql).

%% @doc select
-spec select(ChargeNo :: integer()) -> ChargeList :: [#charge{}].
select(ChargeNo) ->
    Sql = parser:format(?SELECT_CHARGE, [ChargeNo]),
    Data = db:select(Sql),
    parser:convert(Data, charge).

%% @doc update
-spec update(Charge :: #charge{}) -> AffectedRows :: non_neg_integer().
update(Charge) ->
    Sql = <<(parser:format(element(1, ?UPDATE_CHARGE), Charge))/binary, (parser:format(element(2, ?UPDATE_CHARGE), [Charge#charge.charge_no]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(ChargeNo :: integer()) -> AffectedRows :: non_neg_integer().
delete(ChargeNo) ->
    Sql = parser:format(?DELETE_CHARGE, [ChargeNo]),
    db:delete(Sql).

%% @doc update
-spec update_status(UpdateStatus :: integer(), ChargeNo :: integer()) -> non_neg_integer().
update_status(UpdateStatus, ChargeNo) ->
    Sql = parser:format(?UPDATE_STATUS, [UpdateStatus, ChargeNo]),
    db:update(Sql).

%% @doc delete
-spec delete_in_charge_no(ChargeNoList :: [ChargeNo :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_charge_no(ChargeNoList) ->
    Sql = parser:collect(ChargeNoList, ?DELETE_IN_CHARGE_NO),
    db:delete(Sql).

