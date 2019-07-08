-module(vip_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").


read(10301, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10301, [#vip{level = Level, exp = Exp, expire_time = ExpireTime}]) ->
    VipBinary = <<Level:8, Exp:64, ExpireTime:32>>,
    {ok, protocol:pack(10301, <<VipBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
