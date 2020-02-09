-module(war_protocol).
-export([read/2, write/2]).


read(18001, <<MonsterId:32>>) ->
    {ok, MonsterId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(18001, Result) ->
    {ok, protocol:pack(18001, <<(text(18001, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(18001, no_such_boss, sc) ->
    <<13:16, "没有此Boss"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

