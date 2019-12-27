-module(key_protocol).
-export([read/2, write/2]).


read(15001, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, Key};

read(Code, Binary) ->
    {error, Code, Binary}.



write(15001, Result) ->
    {ok, protocol:pack(15001, <<(text(15001, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(15001, key_already_active) ->
    <<30:16, "此兑换码已经兑换过了"/utf8>>;
text(15001, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(_, 0) ->
    <<0:16>>;
text(_, ok) ->
    <<0:16>>;
text(_, Reason) ->
    <<(protocol:write_bit_string(type:to_binary(Reason)))/binary>>.

