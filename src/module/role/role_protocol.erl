-module(role_protocol).
-export([read/2, write/2]).
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").


read(10101, <<>>) ->
    {ok, []};

read(10102, <<>>) ->
    {ok, []};

read(10103, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.


write(10101, #role{role_id = RoleId, role_name = RoleName, sex = Sex, classes = Classes, level = Level, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}) ->
    {ok, protocol:pack(10101, <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, Level:64, ItemSize:16, BagSize:16, StoreSize:16>>)};

write(10102, #asset{gold = Gold, silver = Silver, copper = Copper, exp = Exp}) ->
    {ok, protocol:pack(10102, <<Gold:64, Silver:32, Copper:64, Exp:64>>)};

write(10103, #vip{vip_level = VipLevel, exp = Exp, expire_time = ExpireTime}) ->
    {ok, protocol:pack(10103, <<VipLevel:8, Exp:64, ExpireTime:32>>)};

write(Code, Content) ->
    {error, Code, Content}.


