-module(vip_protocol).
-export([read/2, write/2]).
-include("vip.hrl").


read(10301, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10301, #vip{vip_level = VipLevel, exp = Exp, expire_time = ExpireTime}) ->
    {ok, protocol:pack(10301, <<VipLevel:8, Exp:64, ExpireTime:32>>)};

write(Code, Content) ->
    {error, Code, Content}.

