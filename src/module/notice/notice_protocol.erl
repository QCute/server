-module(notice_protocol).
-compile(nowarn_export_all).
-compile(export_all).


read(50001, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(50001, [Scope, Type, Msg]) ->
    {ok, protocol:pack(50001, <<Scope:8, Type:8, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
