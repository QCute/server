-module(cheat_protocol).
-export([read/2, write/2]).


read(60001, <<>>) ->
    {ok, []};

read(60002, <<CommandLength:16, Command:CommandLength/binary>>) ->
    CommandString = binary_to_list(Command),
    {ok, CommandString};

read(Code, Binary) ->
    {error, Code, Binary}.



write(60001, CheatList) ->
    {ok, protocol:pack(60001, <<(length(CheatList)):16, <<<<(length(Description)):16, (list_to_binary(Description))/binary, (length(Command)):16, (list_to_binary(Command))/binary>> || {Description, Command} <- CheatList>>/binary>>)};

write(60002, Result) ->
    {ok, protocol:pack(60002, <<(text(60002, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(60002, no_such_command, sc) ->
    <<18:16, "没有找到命令"/utf8>>;
text(_, Reason, _) ->
    protocol:write_bit_string(type:to_binary(Reason)).

