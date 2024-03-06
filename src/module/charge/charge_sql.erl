-module(charge_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("charge.hrl").

%% @doc insert into charge
-spec insert(Charge :: #charge{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#charge{role_id = RoleId, first_charge_time = FirstChargeTime, last_charge_time = LastChargeTime, daily_total = DailyTotal, weekly_total = WeeklyTotal, monthly_total = MonthlyTotal, charge_total = ChargeTotal}) ->
    db:insert(<<"INSERT INTO `charge` (`role_id`, `first_charge_time`, `last_charge_time`, `daily_total`, `weekly_total`, `monthly_total`, `charge_total`) VALUES (?, ?, ?, ?, ?, ?, ?)">>, [RoleId, FirstChargeTime, LastChargeTime, DailyTotal, WeeklyTotal, MonthlyTotal, ChargeTotal]).

%% @doc select from charge
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#charge{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `first_charge_time`, `last_charge_time`, `daily_total`, `weekly_total`, `monthly_total`, `charge_total` FROM `charge` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, charge).

%% @doc update into charge
-spec update(#charge{}) -> AffectedRows :: non_neg_integer().
update(#charge{role_id = RoleId, first_charge_time = FirstChargeTime, last_charge_time = LastChargeTime, daily_total = DailyTotal, weekly_total = WeeklyTotal, monthly_total = MonthlyTotal, charge_total = ChargeTotal}) ->
    db:update(<<"UPDATE `charge` SET `role_id` = ?, `first_charge_time` = ?, `last_charge_time` = ?, `daily_total` = ?, `weekly_total` = ?, `monthly_total` = ?, `charge_total` = ? WHERE `role_id` = ?">>, [RoleId, FirstChargeTime, LastChargeTime, DailyTotal, WeeklyTotal, MonthlyTotal, ChargeTotal, RoleId]).
