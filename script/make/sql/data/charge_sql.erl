-module(charge_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("charge.hrl").

%% @doc insert into charge
-spec insert(Charge :: #charge{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Charge) ->
    db:insert(<<"INSERT INTO `charge` (`role_id`, `first_charge_time`, `last_charge_time`, `daily_total`, `weekly_total`, `monthly_total`, `charge_total`) VALUES (:1:, :2:, :3:, :4:, :5:, :6:, :7:)">>, Charge).

%% @doc select from charge
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#charge{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `first_charge_time`, `last_charge_time`, `daily_total`, `weekly_total`, `monthly_total`, `charge_total` FROM `charge` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, charge).

%% @doc update into charge
-spec update(Charge :: #charge{}) -> AffectedRows :: non_neg_integer().
update(Charge) ->
    db:update(<<"UPDATE `charge` SET `role_id` = :1:, `first_charge_time` = :2:, `last_charge_time` = :3:, `daily_total` = :4:, `weekly_total` = :5:, `monthly_total` = :6:, `charge_total` = :7: WHERE `role_id` = :1:">>, Charge).
