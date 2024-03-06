-module(lucky_money_sql).
-export([save/1]).
-export([select/0]).
-export([delete_in_lucky_money_no/1]).
-include("lucky_money.hrl").

%% @doc insert into lucky_money
-spec save(LuckyMoneyList :: [#lucky_money{}] | ets:tab()) -> NewLuckyMoneyList :: [#lucky_money{}].
save(LuckyMoneyList) ->
    db:save(<<"INSERT INTO `lucky_money` (`lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `scope`, `restrict`, `skin`, `message`, `time`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, LuckyMoneyList, fun(#lucky_money{lucky_money_no = LuckyMoneyNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, total_gold = TotalGold, remain_gold = RemainGold, total_number = TotalNumber, receive_number = ReceiveNumber, scope = Scope, restrict = Restrict, skin = Skin, message = Message, time = Time}) -> [LuckyMoneyNo, ServerId, RoleId, RoleName, GuildId, GuildName, TotalGold, RemainGold, TotalNumber, ReceiveNumber, Scope, Restrict, Skin, Message, Time] end, #lucky_money.flag).

%% @doc select from lucky_money
-spec select() -> Rows :: [#lucky_money{}].
select() ->
    Data = db:select(<<"SELECT `lucky_money_no`, `server_id`, `role_id`, `role_name`, `guild_id`, `guild_name`, `total_gold`, `remain_gold`, `total_number`, `receive_number`, `scope`, `restrict`, `skin`, `message`, `time` FROM `lucky_money`">>, []),
    parser:convert(Data, lucky_money).

%% @doc delete row from lucky_money
-spec delete_in_lucky_money_no(LuckyMoneyNo :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_lucky_money_no(LuckyMoneyNo) ->
    db:delete(<<"DELETE FROM `lucky_money` WHERE `lucky_money_no` IN ?">>, [LuckyMoneyNo]).
