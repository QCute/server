-module(protocol_16).
-compile(nowarn_export_all).
-compile(export_all).


read(16001, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(16002, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(16003, <<UserId:64, MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [UserId, Msg]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(16001, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(16001, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(16002, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(16002, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(16003, [UserId, UserName, Msg]) ->
    UserIdBinary = <<UserId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(16003, <<UserIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
