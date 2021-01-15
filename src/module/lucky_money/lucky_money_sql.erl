-module(lucky_money_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("lucky_money.hrl").
-define(INSERT_LUCKY_MONEY, <<"INSERT INTO `lucky_money` (`server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `time`) VALUES (~i~i~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~i~w~i)">>).
-define(SELECT_LUCKY_MONEY, <<"SELECT `lucky_money_id`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, '' AS `receive_list`, `time`, 0 AS `flag` FROM `lucky_money`">>).
-define(UPDATE_LUCKY_MONEY, {<<"UPDATE `lucky_money` SET ~i~i`server_id` = ~w, `role_id` = ~w, `role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `total_gold` = ~w, `remain_gold` = ~w, `total_number` = ~w, `receive_number` = ~w, ~i`time` = ~w~i ">>, <<"WHERE `lucky_money_id` = ~w">>}).
-define(DELETE_LUCKY_MONEY, <<"DELETE  FROM `lucky_money` WHERE `lucky_money_id` = ~w">>).
-define(INSERT_UPDATE_LUCKY_MONEY, {<<"INSERT INTO `lucky_money` (`lucky_money_id`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~i~w~i)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_id` = VALUES(`role_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `total_gold` = VALUES(`total_gold`), `remain_gold` = VALUES(`remain_gold`), `total_number` = VALUES(`total_number`), `receive_number` = VALUES(`receive_number`), `time` = VALUES(`time`)">>}).
-define(DELETE_IN_LUCKY_MONEY_ID, {<<"DELETE  FROM `lucky_money` WHERE `lucky_money_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(LuckyMoney) ->
    Sql = parser:format(?INSERT_LUCKY_MONEY, LuckyMoney),
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_LUCKY_MONEY, []),
    Data = db:select(Sql),
    parser:convert(Data, lucky_money).

%% @doc update
update(LuckyMoney) ->
    Sql = <<(parser:format(element(1, ?UPDATE_LUCKY_MONEY), LuckyMoney))/binary, (parser:format(element(2, ?UPDATE_LUCKY_MONEY), [LuckyMoney#lucky_money.lucky_money_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(LuckyMoneyId) ->
    Sql = parser:format(?DELETE_LUCKY_MONEY, [LuckyMoneyId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_LUCKY_MONEY, #lucky_money.flag),
    db:insert(Sql),
    NewData.

%% @doc delete
delete_in_lucky_money_id(LuckyMoneyIdList) ->
    Sql = parser:collect(LuckyMoneyIdList, ?DELETE_IN_LUCKY_MONEY_ID),
    db:delete(Sql).

