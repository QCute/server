-module(protocol_50).
-compile(nowarn_export_all).
-compile(export_all).


read(Code, Binary) ->
    {error, Code, Binary}.



write(50001, [Scope, Type, Msg]) ->
    ScopeBinary = <<Scope:8>>,
    TypeBinary = <<Type:8>>,
    MsgBinary = <<(length(Msg)):16, (iolist_to_binary(Msg))/binary>>,
    {ok, protocol:pack(50001, <<ScopeBinary/binary, TypeBinary/binary, MsgBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
