-module(role_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").


read(10101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10101, [#role{role_id = RoleId, role_name = RoleName, account_name = AccountName, account_id = AccountId, sex = Sex, level = Level, classes = Classes, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}]) ->
    RoleBinary = <<RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, (byte_size(AccountName)):16, (AccountName)/binary, (byte_size(AccountId)):16, (AccountId)/binary, Sex:8, Level:64, Classes:8, ItemSize:16, BagSize:16, StoreSize:16>>,
    {ok, protocol:pack(10101, <<RoleBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
