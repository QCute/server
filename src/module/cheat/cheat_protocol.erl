-module(cheat_protocol).
-export([read/2, write/2]).


read(60000, <<CommandLength:16, Command:CommandLength/binary>>) ->
    {ok, binary_to_list(Command)};

read(Code, Binary) ->
    {error, Code, Binary}.



write(60000, [Result, Command]) ->
    {ok, protocol:pack(60000, <<(text(60000, Result))/binary, (byte_size(Command)):16, (list_to_binary(Command))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(60000, no_such_command, sc) ->
    <<18:16, "没有找到命令"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

