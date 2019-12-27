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



write(11601, [UserId, UserName, Msg]) ->
    {ok, protocol:pack(11601, <<UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(11602, [UserId, UserName, Msg]) ->
    {ok, protocol:pack(11602, <<UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(11603, [UserId, UserName, Msg]) ->
    {ok, protocol:pack(11603, <<UserId:64, (byte_size(UserName)):16, (UserName)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

