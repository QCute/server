-module(charge_order_sql).
-export([insert/1]).
-export([select/1]).
-export([update_status/1]).
-include("charge.hrl").

%% @doc insert into charge_order
-spec insert(ChargeOrder :: #charge_order{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(ChargeOrder) ->
    db:insert(<<"INSERT INTO `charge_order` (`charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:)">>, ChargeOrder).

%% @doc select from charge_order
-spec select(ChargeNo :: non_neg_integer()) -> Rows :: [#charge_order{}].
select(ChargeNo) ->
    Data = db:select(<<"SELECT `charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `charge_order` WHERE `charge_no` = ?">>, [ChargeNo]),
    parser:convert(Data, charge_order).

%% @doc update into charge_order
-spec update_status(ChargeOrder :: #charge_order{}) -> AffectedRows :: non_neg_integer().
update_status(ChargeOrder) ->
    db:update(<<"UPDATE `charge_order` SET `status` = :9: WHERE `charge_no` = :1:">>, ChargeOrder).
