-module(key_protocol).
-export([read/2, write/2]).


read(15001, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, Key};

read(Code, Binary) ->
    {error, Code, Binary}.



write(15001, Result) ->
    {ok, protocol:pack(15001, <<Result:8>>)};

write(Code, Content) ->
    {error, Code, Content}.
