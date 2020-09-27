-module(welfare_protocol).
-export([read/2, write/2]).
-include("lucky_money.hrl").


read(15001, <<>>) ->
    {ok, []};

read(15002, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, Key};

read(15003, <<>>) ->
    {ok, []};

read(15004, <<LuckyMoneyId:64>>) ->
    {ok, LuckyMoneyId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(15001, Result) ->
    {ok, protocol:pack(15001, <<(protocol:text(15001, Result))/binary>>)};

write(15002, Result) ->
    {ok, protocol:pack(15002, <<(protocol:text(15002, Result))/binary>>)};

write(15003, List) ->
    ListBinary = protocol:write_ets(fun([#lucky_money{lucky_money_id = LuckyMoneyId, total_gold = TotalGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = SendTime}]) -> <<LuckyMoneyId:64, TotalGold:64, TotalNumber:32, ReceiveNumber:16, (length(ReceiveList)):16, <<<<ServerId:16, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Gold:64, ReceiveTime:32>> || #lucky_money_role{server_id = ServerId, role_id = RoleId, role_name = RoleName, gold = Gold, time = ReceiveTime} <- ReceiveList>>/binary, SendTime:32>> end, List),
    {ok, protocol:pack(15003, <<ListBinary/binary>>)};

write(15004, [Result, Gold]) ->
    {ok, protocol:pack(15004, <<(protocol:text(15004, Result))/binary, Gold:64>>)};

write(15005, []) ->
    {ok, protocol:pack(15005, <<>>)};

write(Code, Content) ->
    {error, Code, Content}.

