-module(lucky_money_role_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("lucky_money.hrl").
-define(INSERT_LUCKY_MONEY_ROLE, <<"INSERT INTO `lucky_money_role` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`) VALUES (~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w~i)">>).
-define(SELECT_LUCKY_MONEY_ROLE, <<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`, 0 AS `flag` FROM `lucky_money_role`">>).
-define(UPDATE_LUCKY_MONEY_ROLE, {<<"UPDATE `lucky_money_role` SET ~i~i`server_id` = ~w, ~i`role_name` = '~s', `guild_id` = ~w, `guild_name` = '~s', `gold` = ~w, `time` = ~w~i ">>, <<"WHERE `lucky_money_no` = ~w AND `role_id` = ~w">>}).
-define(DELETE_LUCKY_MONEY_ROLE, <<"DELETE  FROM `lucky_money_role` WHERE `lucky_money_no` = ~w AND `role_id` = ~w">>).
-define(INSERT_UPDATE_LUCKY_MONEY_ROLE, {<<"INSERT INTO `lucky_money_role` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`) VALUES ">>, <<"(~i~w, ~w, ~w, '~s', ~w, '~s', ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `server_id` = VALUES(`server_id`), `role_name` = VALUES(`role_name`), `guild_id` = VALUES(`guild_id`), `guild_name` = VALUES(`guild_name`), `gold` = VALUES(`gold`), `time` = VALUES(`time`)">>}).
-define(DELETE_BY_LUCKY_MONEY_NO, <<"DELETE FROM `lucky_money_role` WHERE `lucky_money_no` = ~w">>).

%% @doc insert
insert(LuckyMoneyRole) ->
    Sql = parser:format(?INSERT_LUCKY_MONEY_ROLE, LuckyMoneyRole),
    db:insert(Sql).

%% @doc select
select() ->
    Sql = parser:format(?SELECT_LUCKY_MONEY_ROLE, []),
    Data = db:select(Sql),
    parser:convert(Data, lucky_money_role).

%% @doc update
update(LuckyMoneyRole) ->
    Sql = <<(parser:format(element(1, ?UPDATE_LUCKY_MONEY_ROLE), LuckyMoneyRole))/binary, (parser:format(element(2, ?UPDATE_LUCKY_MONEY_ROLE), [LuckyMoneyRole#lucky_money_role.lucky_money_no, LuckyMoneyRole#lucky_money_role.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(LuckyMoneyNo, RoleId) ->
    Sql = parser:format(?DELETE_LUCKY_MONEY_ROLE, [LuckyMoneyNo, RoleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_LUCKY_MONEY_ROLE, #lucky_money_role.flag),
    db:insert(Sql),
    NewData.

%% @doc delete
delete_by_lucky_money_no(LuckyMoneyNo) ->
    Sql = parser:format(?DELETE_BY_LUCKY_MONEY_NO, [LuckyMoneyNo]),
    db:delete(Sql).

