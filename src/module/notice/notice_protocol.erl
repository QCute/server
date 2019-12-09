-module(notice_protocol).
-export([read/2, write/2]).


read(Code, Binary) ->
    {error, Code, Binary}.



write(50001, [Scope, Type, Msg]) ->
    {ok, protocol:pack(50001, <<Scope:8, Type:8, (byte_size(Msg)):16, (list_to_binary(Msg))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
