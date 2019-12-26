-module(cheat_protocol).
-export([read/2, write/2]).


read(60000, <<CommandLength:16, Command:CommandLength/binary>>) ->
    {ok, binary_to_list(Command)};

read(Code, Binary) ->
    {error, Code, Binary}.



write(60000, [Result, Command]) ->
    {ok, protocol:pack(60000, <<Result:8, (byte_size(Command)):16, (list_to_binary(Command))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
