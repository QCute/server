-module(lucky_money_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("lucky_money.hrl").
-define(INSERT_LUCKY_MONEY, <<"INSERT INTO `lucky_money` (`server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `time`) VALUES (~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_LUCKY_MONEY, <<"SELECT `lucky_money_id`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `receive_list`, `time`, `flag` FROM `lucky_money`">>).
-define(UPDATE_LUCKY_MONEY, <<"UPDATE `lucky_money` SET `server_id` = ~w, `role_id` = ~w, `role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `total_gold` = ~w, `remain_gold` = ~w, `total_number` = ~w, `receive_number` = ~w, `time` = ~w WHERE `lucky_money_id` = ~w">>).
-define(DELETE_LUCKY_MONEY, <<"DELETE  FROM `lucky_money` WHERE `lucky_money_id` = ~w">>).
-define(INSERT_UPDATE_LUCKY_MONEY, {<<"INSERT INTO `lucky_money` (`lucky_money_id`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `total_gold` = VALUES(`total_gold`), `remain_gold` = VALUES(`remain_gold`), `total_number` = VALUES(`total_number`), `receive_number` = VALUES(`receive_number`), `time` = VALUES(`time`)">>}).
-define(DELETE_IN_LUCKY_MONEY_ID, {<<"DELETE  FROM `lucky_money` WHERE `lucky_money_id` in (">>, <<"~w">>, <<")">>}).
-define(TRUNCATE, <<"TRUNCATE TABLE `lucky_money`">>).

%% @doc insert
insert(LuckyMoney) ->
    Sql = parser:format(?INSERT_LUCKY_MONEY, [
        LuckyMoney#lucky_money.server_id,
        LuckyMoney#lucky_money.role_id,
        LuckyMoney#lucky_money.role_name,
        LuckyMoney#lucky_money.guild_id,
        LuckyMoney#lucky_money.guild_name,
        LuckyMoney#lucky_money.total_gold,
        LuckyMoney#lucky_money.remain_gold,
        LuckyMoney#lucky_money.total_number,
        LuckyMoney#lucky_money.receive_number,
        LuckyMoney#lucky_money.time
    ]),
    sql:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_LUCKY_MONEY, []),
    Data = sql:select(Sql),
    F = fun(LuckyMoney = #lucky_money{receive_list = ReceiveList}) -> LuckyMoney#lucky_money{receive_list = parser:to_term(ReceiveList)} end,
    parser:convert(Data, lucky_money, F).

%% @doc update
update(LuckyMoney) ->
    Sql = parser:format(?UPDATE_LUCKY_MONEY, [
        LuckyMoney#lucky_money.server_id,
        LuckyMoney#lucky_money.role_id,
        LuckyMoney#lucky_money.role_name,
        LuckyMoney#lucky_money.guild_id,
        LuckyMoney#lucky_money.guild_name,
        LuckyMoney#lucky_money.total_gold,
        LuckyMoney#lucky_money.remain_gold,
        LuckyMoney#lucky_money.total_number,
        LuckyMoney#lucky_money.receive_number,
        LuckyMoney#lucky_money.time,
        LuckyMoney#lucky_money.lucky_money_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(LuckyMoneyId) ->
    Sql = parser:format(?DELETE_LUCKY_MONEY, [LuckyMoneyId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(LuckyMoney) -> [
        LuckyMoney#lucky_money.lucky_money_id,
        LuckyMoney#lucky_money.server_id,
        LuckyMoney#lucky_money.role_id,
        LuckyMoney#lucky_money.role_name,
        LuckyMoney#lucky_money.guild_id,
        LuckyMoney#lucky_money.guild_name,
        LuckyMoney#lucky_money.total_gold,
        LuckyMoney#lucky_money.remain_gold,
        LuckyMoney#lucky_money.total_number,
        LuckyMoney#lucky_money.receive_number,
        LuckyMoney#lucky_money.time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_LUCKY_MONEY, #lucky_money.flag),
    sql:insert(Sql),
    NewData.

%% @doc delete
delete_in_lucky_money_id(LuckyMoneyIdList) ->
    F = fun(LuckyMoneyId) -> [LuckyMoneyId] end,
    Sql = parser:collect(LuckyMoneyIdList, F, ?DELETE_IN_LUCKY_MONEY_ID),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

