-module(charge_order_sql).
-export([insert/1]).
-export([select/1]).
-export([update_status/1]).
-include("charge.hrl").

%% @doc insert into charge_order
-spec insert(ChargeOrder :: #charge_order{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#charge_order{charge_no = ChargeNo, charge_id = ChargeId, order_id = OrderId, channel = Channel, role_id = RoleId, role_name = RoleName, money = Money, status = Status, time = Time}) ->
    db:insert(<<"INSERT INTO `charge_order` (`charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)">>, [ChargeNo, ChargeId, OrderId, Channel, RoleId, RoleName, Money, Status, Time]).

%% @doc select from charge_order
-spec select(ChargeNo :: non_neg_integer()) -> Rows :: [#charge_order{}].
select(ChargeNo) ->
    Data = db:select(<<"SELECT `charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `charge_order` WHERE `charge_no` = ?">>, [ChargeNo]),
    parser:convert(Data, charge_order).

%% @doc update into charge_order
-spec update_status(#charge_order{}) -> AffectedRows :: non_neg_integer().
update_status(#charge_order{status = Status, charge_no = ChargeNo}) ->
    db:update(<<"UPDATE `charge_order` SET `status` = ? WHERE `charge_no` = ?">>, [Status, ChargeNo]).
