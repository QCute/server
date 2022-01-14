-module(welfare_protocol).
-export([read/2, write/2]).
-include("lucky_money.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(15001, <<>>) ->
    {ok, []};

read(15002, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, Key};

read(15003, <<LuckyMoneyNo:64>>) ->
    {ok, LuckyMoneyNo};

read(15004, <<LuckyMoneyNo:64>>) ->
    {ok, LuckyMoneyNo};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(15001, Result) ->
    {ok, protocol:pack(15001, <<(protocol:text(Result))/binary>>)};

write(15002, Result) ->
    {ok, protocol:pack(15002, <<(protocol:text(Result))/binary>>)};

write(15003, #lucky_money{lucky_money_no = LuckyMoneyNo, total_gold = TotalGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = SendTime}) ->
    ReceiveListBinary = protocol:write_list(fun(#lucky_money_role{server_id = ServerId, role_id = RoleId, role_name = RoleName, gold = Gold, time = ReceiveTime}) -> <<ServerId:16, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Gold:64, ReceiveTime:32>> end, ReceiveList),
    {ok, protocol:pack(15003, <<LuckyMoneyNo:64, TotalGold:64, TotalNumber:32, ReceiveNumber:16, ReceiveListBinary/binary, SendTime:32>>)};

write(15004, [Result, Gold]) ->
    {ok, protocol:pack(15004, <<(protocol:text(Result))/binary, Gold:64>>)};

write(15005, []) ->
    {ok, protocol:pack(15005, <<>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


