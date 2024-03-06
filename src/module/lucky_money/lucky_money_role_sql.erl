-module(lucky_money_role_sql).
-export([save/1]).
-export([select/0]).
-export([delete_by_lucky_money_no/1]).
-include("lucky_money.hrl").

%% @doc insert into lucky_money_role
-spec save(LuckyMoneyRoleList :: [#lucky_money_role{}] | ets:tab()) -> NewLuckyMoneyRoleList :: [#lucky_money_role{}].
save(LuckyMoneyRoleList) ->
    db:save(<<"INSERT INTO `lucky_money_role` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, LuckyMoneyRoleList, fun(#lucky_money_role{lucky_money_no = LuckyMoneyNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, gold = Gold, time = Time}) -> [LuckyMoneyNo, ServerId, RoleId, RoleName, GuildId, GuildName, Gold, Time] end, #lucky_money_role.flag).

%% @doc select from lucky_money_role
-spec select() -> Rows :: [#lucky_money_role{}].
select() ->
    Data = db:select(<<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `gold`, `time` FROM `lucky_money_role`">>, []),
    parser:convert(Data, lucky_money_role).

%% @doc delete row from lucky_money_role
-spec delete_by_lucky_money_no(LuckyMoneyNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_by_lucky_money_no(LuckyMoneyNo) ->
    db:delete(<<"DELETE FROM `lucky_money_role` WHERE `lucky_money_no` = ?">>, [LuckyMoneyNo]).
