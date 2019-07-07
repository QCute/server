-module(chat_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(11601, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(11602, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(11603, <<UserId:64, MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [UserId, Msg]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11601, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11601, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(11602, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11602, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(11603, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11603, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
