-module(cheat_protocol).
-export([read/2, write/2]).


read(60000, <<CommandLength:16, Command:CommandLength/binary>>) ->
    CommandString = binary_to_list(Command),
    {ok, CommandString};

read(60001, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(60000, Result) ->
    {ok, protocol:pack(60000, <<(text(60000, Result))/binary>>)};

write(60001, CheatList) ->
    {ok, protocol:pack(60001, <<(length(CheatList)):16, <<<<(length(Description)):16, (list_to_binary(Description))/binary, (length(Command)):16, (list_to_binary(Command))/binary>> || {Description, Command} <- CheatList>>/binary>>)};

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

