-module(lucky_money_role_sql).
-export([save/1]).
-export([select/0]).
-export([delete_by_lucky_money_no/1]).
-include("lucky_money.hrl").

%% @doc insert into lucky_money_role
-spec save(LuckyMoneyRoleList :: [#lucky_money_role{}] | ets:tab()) -> NewLuckyMoneyRoleList :: [#lucky_money_role{}].
save(LuckyMoneyRoleList) ->
    db:save_into(<<"INSERT INTO `lucky_money_role` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:)">>, <<"ON DUPLICATE KEY UPDATE `lucky_money_no` = VALUES(`lucky_money_no`), `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `gold` = VALUES(`gold`), `time` = VALUES(`time`)">>, LuckyMoneyRoleList, #lucky_money_role.flag).

%% @doc select from lucky_money_role
-spec select() -> Rows :: [#lucky_money_role{}].
select() ->
    Data = db:select(<<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`, `flag` FROM `lucky_money_role`">>, []),
    parser:convert(Data, lucky_money_role).

%% @doc delete row from lucky_money_role
-spec delete_by_lucky_money_no(LuckyMoneyNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_lucky_money_no(LuckyMoneyNo) ->
    db:delete(<<"DELETE FROM `lucky_money_role` WHERE `lucky_money_no` = ?">>, [LuckyMoneyNo]).
