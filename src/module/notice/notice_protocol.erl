-module(notice_protocol).
-export([read/2, write/2]).


read(Code, Binary) ->
    {error, Code, Binary}.


write(50001, [Scope, Type, Title, Msg]) ->
    {ok, protocol:pack(50001, <<Scope:8, Type:8, (byte_size(Title)):16, (Title)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


