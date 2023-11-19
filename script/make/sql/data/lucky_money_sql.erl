-module(lucky_money_sql).
-export([save/1]).
-export([select/0]).
-export([delete_in_lucky_money_no/1]).
-include("lucky_money.hrl").

%% @doc insert into lucky_money
-spec save(LuckyMoneyList :: [#lucky_money{}] | ets:tab()) -> NewLuckyMoneyList :: [#lucky_money{}].
save(LuckyMoneyList) ->
    db:save_into(<<"INSERT INTO `lucky_money` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `scope`, `restrict`, `skin`, `message`, `time`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:, :13:, :14:, :15:, :16:, :17:)">>, <<"ON DUPLICATE KEY UPDATE `lucky_money_no` = VALUES(`lucky_money_no`), `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `total_gold` = VALUES(`total_gold`), `remain_gold` = VALUES(`remain_gold`), `total_number` = VALUES(`total_number`), `receive_number` = VALUES(`receive_number`), `scope` = VALUES(`scope`), `restrict` = VALUES(`restrict`), `skin` = VALUES(`skin`), `message` = VALUES(`message`), `time` = VALUES(`time`)">>, LuckyMoneyList, #lucky_money.flag).

%% @doc select from lucky_money
-spec select() -> Rows :: [#lucky_money{}].
select() ->
    Data = db:select(<<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `receive_list`, `scope`, `restrict`, `skin`, `message`, `time`, `flag` FROM `lucky_money`">>, []),
    parser:convert(Data, lucky_money, fun(LuckyMoney = #lucky_money{scope = Scope}) -> LuckyMoney#lucky_money{scope = parser:to_term(Scope)} end).

%% @doc delete row from lucky_money
-spec delete_in_lucky_money_no(LuckyMoneyNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_lucky_money_no(LuckyMoneyNo) ->
    db:delete(<<"DELETE FROM `lucky_money` WHERE `lucky_money_no` IN (?)">>, [LuckyMoneyNo]).
