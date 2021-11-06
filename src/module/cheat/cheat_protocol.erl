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
    CheatListBinary = protocol:write_list(fun({Description, Command}) -> <<(length(Description)):16, (list_to_binary(Description))/binary, (length(Command)):16, (list_to_binary(Command))/binary>> end, CheatList),
    {ok, protocol:pack(60001, <<CheatListBinary/binary>>)};

write(60002, Result) ->
    {ok, protocol:pack(60002, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


