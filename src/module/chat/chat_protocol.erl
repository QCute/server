-module(chat_protocol).
-export([read/2, write/2]).


read(11601, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, Msg};

read(11602, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, Msg};

read(11603, <<UserId:64, MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [UserId, Msg]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11601, [Result, UserId, UserName, Msg]) ->
    {ok, protocol:pack(11601, <<(text(11601, Result))/binary, UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(11602, [Result, UserId, UserName, Msg]) ->
    {ok, protocol:pack(11602, <<(text(11602, Result))/binary, UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(11603, [Result, UserId, UserName, Msg]) ->
    {ok, protocol:pack(11603, <<(text(11603, Result))/binary, UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(11601, level_not_enough, sc) ->
    <<12:16, "等级不足"/utf8>>;
text(11601, time_in_cd, sc) ->
    <<15:16, "时间冷却中"/utf8>>;
text(11602, level_not_enough, sc) ->
    <<12:16, "等级不足"/utf8>>;
text(11602, no_guild, sc) ->
    <<15:16, "没加入公会"/utf8>>;
text(11602, time_in_cd, sc) ->
    <<15:16, "时间冷却中"/utf8>>;
text(11603, level_not_enough, sc) ->
    <<12:16, "等级不足"/utf8>>;
text(11603, user_offline, sc) ->
    <<15:16, "对方不在线"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

