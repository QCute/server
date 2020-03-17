-module(welfare_protocol).
-export([read/2, write/2]).
-include("lucky_money.hrl").


read(15001, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, Key};

read(15002, <<>>) ->
    {ok, []};

read(15003, <<LuckyMoneyId:64>>) ->
    {ok, LuckyMoneyId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(15001, Result) ->
    {ok, protocol:pack(15001, <<(text(15001, Result))/binary>>)};

write(15002, List) ->
    ListBinary = protocol:write_ets(fun([#lucky_money{lucky_money_id = LuckyMoneyId, total_gold = TotalGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = SendTime}]) -> <<LuckyMoneyId:64, TotalGold:64, TotalNumber:32, ReceiveNumber:16, (length(ReceiveList)):16, <<<<ServerId:16, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Gold:64, ReceiveTime:32>> || #lucky_money_role{server_id = ServerId, role_id = RoleId, role_name = RoleName, gold = Gold, time = ReceiveTime} <- ReceiveList>>/binary, SendTime:32>> end, List),
    {ok, protocol:pack(15002, <<ListBinary/binary>>)};

write(15003, [Result, Gold]) ->
    {ok, protocol:pack(15003, <<(text(15003, Result))/binary, Gold:64>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(15001, key_already_active, sc) ->
    <<30:16, "此兑换码已经兑换过了"/utf8>>;
text(15001, timeout, sc) ->
    <<12:16, "请求超时"/utf8>>;
text(15003, lucky_money_already_receive, sc) ->
    <<18:16, "红包已领取过"/utf8>>;
text(15003, lucky_money_expire, sc) ->
    <<15:16, "红包已过期"/utf8>>;
text(15003, no_such_lucky_money, sc) ->
    <<30:16, "此兑换码已经兑换过了"/utf8>>;
text(15003, timeout, sc) ->
    <<12:16, "请求超时"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).
