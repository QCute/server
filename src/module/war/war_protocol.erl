-module(war_protocol).
-export([read/2, write/2]).


read(18001, <<MonsterId:32>>) ->
    {ok, MonsterId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(18001, Result) ->
    {ok, protocol:pack(18001, <<(protocol:text(18001, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

