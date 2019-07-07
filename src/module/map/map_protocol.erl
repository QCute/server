-module(map_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(20001, <<>>) ->
    {ok, []};

read(20002, <<X:16, Y:16>>) ->
    {ok, [X, Y]};

read(20003, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(20001, []) ->
    {ok, protocol:pack(20001, <<>>)};

write(20002, [X, Y]) ->
    XBinary = <<X:16>>,
    YBinary = <<Y:16>>,
    {ok, protocol:pack(20002, <<XBinary/binary, YBinary/binary>>)};

write(20003, [X, Y]) ->
    XBinary = <<X:16>>,
    YBinary = <<Y:16>>,
    {ok, protocol:pack(20003, <<XBinary/binary, YBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
