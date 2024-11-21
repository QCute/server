-module(welfare_protocol).
-export([decode/2, encode/2]).
-include("lucky_money.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(15001, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(15002, _Rest_ = <<_/binary>>) ->
    <<DataLength:16, Data:DataLength/binary, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(15003, _Rest_ = <<_/binary>>) ->
    <<Data:64, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(15004, _Rest_ = <<_/binary>>) ->
    <<Data:64, _DataRest_/binary>> = _Rest_,
    {ok, Data};


decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(15001, Data) ->
    Data15001 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data15001)):16, 15001:16, Data15001/binary>>};

encode(15002, Data) ->
    Data15002 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data15002)):16, 15002:16, Data15002/binary>>};

encode(15003, #lucky_money{lucky_money_no = LuckyMoneyNo, total_gold = TotalGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = Time}) ->
    Data15003 = <<LuckyMoneyNo:64, TotalGold:64, TotalNumber:32, ReceiveNumber:16, (encode_receive_list_15003(<<>>, 0, ReceiveList))/binary, Time:32>>,
    {ok, <<(byte_size(Data15003)):16, 15003:16, Data15003/binary>>};

encode(15004, {Result, Gold}) ->
    Data15004 = <<(protocol:text(Result))/binary, Gold:64>>,
    {ok, <<(byte_size(Data15004)):16, 15004:16, Data15004/binary>>};

encode(15005, {}) ->
    Data15005 = <<>>,
    {ok, <<(byte_size(Data15005)):16, 15005:16, Data15005/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_receive_list_15003(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_receive_list_15003(Acc = <<_/binary>>, Length, [#lucky_money_role{server_id = ReceiveListServerId, role_id = ReceiveListRoleId, role_name = ReceiveListRoleName, gold = ReceiveListGold, time = ReceiveListTime} | ReceiveList]) ->
    encode_receive_list_15003(<<Acc/binary, ReceiveListServerId:16, ReceiveListRoleId:64, (byte_size(ReceiveListRoleName)):16, (ReceiveListRoleName)/binary, ReceiveListGold:64, ReceiveListTime:32>>, Length + 1, ReceiveList).

