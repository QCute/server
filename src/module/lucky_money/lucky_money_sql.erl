-module(lucky_money_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/1]).
-export([insert_update/1]).
-export([delete_in_lucky_money_no/1]).
-include("lucky_money.hrl").

-define(INSERT_LUCKY_MONEY, <<"INSERT INTO `lucky_money` (`server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `scope`, `restrict`, `skin`, `message`, `time`) VALUES (~i~i~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~i'~w', ~w, ~w, '~s', ~w~i)">>).
-define(SELECT_LUCKY_MONEY, <<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, '' AS `receive_list`, `scope`, `restrict`, `skin`, `message`, `time`, 0 AS `flag` FROM `lucky_money`">>).
-define(UPDATE_LUCKY_MONEY, {<<"UPDATE `lucky_money` SET ~i~i`server_id` = ~w, `role_id` = ~w, `role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `total_gold` = ~w, `remain_gold` = ~w, `total_number` = ~w, `receive_number` = ~w, ~i`scope` = '~w', `restrict` = ~w, `skin` = ~w, `message` = '~s', `time` = ~w~i ">>, <<"WHERE `lucky_money_no` = ~w">>}).
-define(DELETE_LUCKY_MONEY, <<"DELETE FROM `lucky_money` WHERE `lucky_money_no` = ~w">>).
-define(INSERT_UPDATE_LUCKY_MONEY, {<<"INSERT INTO `lucky_money` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `scope`, `restrict`, `skin`, `message`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~i'~w', ~w, ~w, '~s', ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `total_gold` = VALUES(`total_gold`), `remain_gold` = VALUES(`remain_gold`), `total_number` = VALUES(`total_number`), `receive_number` = VALUES(`receive_number`), `scope` = VALUES(`scope`), `restrict` = VALUES(`restrict`), `skin` = VALUES(`skin`), `message` = VALUES(`message`), `time` = VALUES(`time`)">>}).
-define(DELETE_IN_LUCKY_MONEY_NO, {<<"DELETE FROM `lucky_money` WHERE `lucky_money_no` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(LuckyMoney :: #lucky_money{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(LuckyMoney) ->
    Sql = parser:format(?INSERT_LUCKY_MONEY, LuckyMoney),
    db:insert(Sql).

%% @doc select
-spec select() -> LuckyMoneyList :: [#lucky_money{}].
select() ->
    Sql = parser:format(?SELECT_LUCKY_MONEY, []),
    Data = db:select(Sql),
    F = fun(LuckyMoney = #lucky_money{scope = Scope}) -> LuckyMoney#lucky_money{scope = parser:to_term(Scope)} end,
    parser:convert(Data, lucky_money, F).

%% @doc update
-spec update(LuckyMoney :: #lucky_money{}) -> AffectedRows :: non_neg_integer().
update(LuckyMoney) ->
    Sql = <<(parser:format(element(1, ?UPDATE_LUCKY_MONEY), LuckyMoney))/binary, (parser:format(element(2, ?UPDATE_LUCKY_MONEY), [LuckyMoney#lucky_money.lucky_money_no]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(LuckyMoneyNo :: integer()) -> AffectedRows :: non_neg_integer().
delete(LuckyMoneyNo) ->
    Sql = parser:format(?DELETE_LUCKY_MONEY, [LuckyMoneyNo]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(LuckyMoneyList :: [#lucky_money{}] | ets:tab()) -> NewLuckyMoneyList :: [#lucky_money{}].
insert_update(LuckyMoneyList) ->
    {Sql, NewLuckyMoneyList} = parser:collect_into(LuckyMoneyList, ?INSERT_UPDATE_LUCKY_MONEY, #lucky_money.flag),
    db:insert(Sql),
    NewLuckyMoneyList.

%% @doc delete
-spec delete_in_lucky_money_no(LuckyMoneyNoList :: [LuckyMoneyNo :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_lucky_money_no(LuckyMoneyNoList) ->
    Sql = parser:collect(LuckyMoneyNoList, ?DELETE_IN_LUCKY_MONEY_NO),
    db:delete(Sql).

