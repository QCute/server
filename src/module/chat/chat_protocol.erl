-module(chat_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(11601, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(11602, <<MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [Msg]};

read(11603, <<RoleId:64, MsgLength:16, Msg:MsgLength/binary>>) ->
    {ok, [RoleId, Msg]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11601, [RoleId, UserName, Msg]) ->
    RoleIdBinary = <<RoleId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11601, <<RoleIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(11602, [RoleId, UserName, Msg]) ->
    RoleIdBinary = <<RoleId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11602, <<RoleIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(11603, [RoleId, UserName, Msg]) ->
    RoleIdBinary = <<RoleId:64>>,
    UserNameBinary = <<(byte_size(UserName)):16, (UserName)/binary>>,
    MsgBinary = <<(byte_size(Msg)):16, (Msg)/binary>>,
    {ok, protocol:pack(11603, <<RoleIdBinary/binary, UserNameBinary/binary, MsgBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
