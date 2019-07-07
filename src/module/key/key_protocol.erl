-module(key_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(15001, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, [Key]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(15001, [Result]) ->
    ResultBinary = <<Result:8>>,
    {ok, protocol:pack(15001, <<ResultBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
