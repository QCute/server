-module(role_protocol).
-export([read/2, write/2]).
-include("role.hrl").


read(10101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10101, [#role{role_id = RoleId, role_name = RoleName, account_id = AccountId, account_name = AccountName, sex = Sex, level = Level, classes = Classes, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}]) ->
    {ok, protocol:pack(10101, <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, (byte_size(AccountId)):16, (AccountId)/binary, (byte_size(AccountName)):16, (AccountName)/binary, Sex:8, Level:64, Classes:8, ItemSize:16, BagSize:16, StoreSize:16>>)};

write(Code, Content) ->
    {error, Code, Content}.
