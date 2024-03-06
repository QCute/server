-module(charge_sql).
-export([insert/1]).
-export([select/1]).
-export([update_status/1]).
-include("charge.hrl").

%% @doc insert into charge
-spec insert(Charge :: #charge{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#charge{charge_no = ChargeNo, charge_id = ChargeId, order_id = OrderId, channel = Channel, role_id = RoleId, role_name = RoleName, money = Money, status = Status, time = Time}) ->
    db:insert(<<"INSERT INTO `charge` (`charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)">>, [ChargeNo, ChargeId, OrderId, Channel, RoleId, RoleName, Money, Status, Time]).

%% @doc select from charge
-spec select(ChargeNo :: non_neg_integer()) -> Rows :: [#charge{}].
select(ChargeNo) ->
    Data = db:select(<<"SELECT `charge_no`, `charge_id`, `order_id`, `channel`, `role_id`, `role_name`, `money`, `status`, `time` FROM `charge` WHERE `charge_no` = ?">>, [ChargeNo]),
    parser:convert(Data, charge).

%% @doc update into charge
-spec update_status(#charge{}) -> AffectedRows :: non_neg_integer().
update_status(#charge{status = Status, charge_no = ChargeNo}) ->
    db:update(<<"UPDATE `charge` SET `status` = ? WHERE `charge_no` = ?">>, [Status, ChargeNo]).
