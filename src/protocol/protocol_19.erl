-module(protocol_19).
-compile(nowarn_export_all).
-compile(export_all).


read(19001, <<KeyLength:16, Key:KeyLength/binary>>) ->
    {ok, [Key]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(19001, [Result]) ->
    ResultBinary = <<Result:8>>,
    {ok, protocol:pack(19001, <<ResultBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
