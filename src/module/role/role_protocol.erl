-module(role_protocol).
-export([read/2, write/2]).
-include("role.hrl").


read(10101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10101, #role{role_id = RoleId, role_name = RoleName, level = Level, sex = Sex, classes = Classes, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}) ->
    {ok, protocol:pack(10101, <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Level:64, Sex:8, Classes:8, ItemSize:16, BagSize:16, StoreSize:16>>)};

write(Code, Content) ->
    {error, Code, Content}.
