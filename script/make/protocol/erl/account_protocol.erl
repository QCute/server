-module(account_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(10000, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(10001, _Rest_ = <<_/binary>>) ->
    <<ServerId:16, _ServerIdRest_/binary>> = _Rest_,
    <<AccountNameLength:16, AccountName:AccountNameLength/binary, _AccountNameRest_/binary>> = _ServerIdRest_,
    {ok, {ServerId, AccountName}};

decode(10002, _Rest_ = <<_/binary>>) ->
    <<RoleNameLength:16, RoleName:RoleNameLength/binary, _RoleNameRest_/binary>> = _Rest_,
    <<ServerId:16, _ServerIdRest_/binary>> = _RoleNameRest_,
    <<AccountNameLength:16, AccountName:AccountNameLength/binary, _AccountNameRest_/binary>> = _ServerIdRest_,
    <<Sex:8, _SexRest_/binary>> = _AccountNameRest_,
    <<Classes:8, _ClassesRest_/binary>> = _SexRest_,
    <<ChannelLength:16, Channel:ChannelLength/binary, _ChannelRest_/binary>> = _ClassesRest_,
    <<DeviceIdLength:16, DeviceId:DeviceIdLength/binary, _DeviceIdRest_/binary>> = _ChannelRest_,
    <<MacLength:16, Mac:MacLength/binary, _MacRest_/binary>> = _DeviceIdRest_,
    <<DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary, _DeviceTypeRest_/binary>> = _MacRest_,
    {ok, {RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType}};

decode(10003, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<RoleNameLength:16, RoleName:RoleNameLength/binary, _RoleNameRest_/binary>> = _RoleIdRest_,
    <<ServerId:16, _ServerIdRest_/binary>> = _RoleNameRest_,
    <<AccountNameLength:16, AccountName:AccountNameLength/binary, _AccountNameRest_/binary>> = _ServerIdRest_,
    {ok, {RoleId, RoleName, ServerId, AccountName}};

decode(10004, _Rest_ = <<_/binary>>) ->
    {ok, {}};


decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(10000, Data) ->
    Data10000 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data10000)):16, 10000:16, Data10000/binary>>};

encode(10001, {Result, List}) ->
    Data10001 = <<(protocol:text(Result))/binary, (encode_list_10001(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data10001)):16, 10001:16, Data10001/binary>>};

encode(10002, {Result, RoleId, RoleName}) ->
    Data10002 = <<(protocol:text(Result))/binary, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary>>,
    {ok, <<(byte_size(Data10002)):16, 10002:16, Data10002/binary>>};

encode(10003, Data) ->
    Data10003 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data10003)):16, 10003:16, Data10003/binary>>};

encode(10004, Data) ->
    Data10004 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data10004)):16, 10004:16, Data10004/binary>>};


encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_10001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_10001(Acc = <<_/binary>>, Length, [{ListRoleId, ListRoleName} | List]) ->
    encode_list_10001(<<Acc/binary, ListRoleId:64, (byte_size(ListRoleName)):16, (ListRoleName)/binary>>, Length + 1, List).

