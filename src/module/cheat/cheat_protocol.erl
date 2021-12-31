-module(cheat_protocol).
-export([read/2, write/2]).


read(60001, <<>>) ->
    {ok, []};

read(60002, <<CommandLength:16, Command:CommandLength/binary>>) ->
    {ok, Command};

read(Code, Binary) ->
    {error, Code, Binary}.


write(60001, CheatList) ->
    CheatListBinary = protocol:write_list(fun({Description, Command}) -> <<(byte_size(Description)):16, (Description)/binary, (byte_size(Command)):16, (Command)/binary>> end, CheatList),
    {ok, protocol:pack(60001, <<CheatListBinary/binary>>)};

write(60002, Result) ->
    {ok, protocol:pack(60002, <<(protocol:text(Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


