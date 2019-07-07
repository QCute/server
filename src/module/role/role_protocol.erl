-module(role_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").


read(10101, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10101, [#role{id = Id, sex = Sex, level = Level, classes = Classes, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize}]) ->
    RoleBinary = <<Id:64, Sex:8, Level:64, Classes:8, ItemSize:16, BagSize:16, StoreSize:16>>,
    {ok, protocol:pack(10101, <<RoleBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
