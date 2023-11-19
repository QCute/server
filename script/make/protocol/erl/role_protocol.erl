-module(role_protocol).
-export([decode/2, encode/2]).
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(10101, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(10102, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(10103, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(10101, #role{role_id = RoleId, role_name = RoleName, sex = Sex, classes = Classes, level = Level}) ->
    Data10101 = <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, Level:64>>,
    {ok, <<(byte_size(Data10101)):16, 10101:16, Data10101/binary>>};

encode(10102, #asset{gold = Gold, silver = Silver, copper = Copper, exp = Exp}) ->
    Data10102 = <<Gold:64, Silver:32, Copper:64, Exp:64>>,
    {ok, <<(byte_size(Data10102)):16, 10102:16, Data10102/binary>>};

encode(10103, #vip{vip_level = VipLevel, exp = Exp, expire_time = ExpireTime}) ->
    Data10103 = <<VipLevel:8, Exp:64, ExpireTime:32>>,
    {ok, <<(byte_size(Data10103)):16, 10103:16, Data10103/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

