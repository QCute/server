-module(account_protocol).
-export([read/2, write/2]).


read(10000, <<>>) ->
    {ok, []};

read(10001, <<ServerId:16, AccountLength:16, Account:AccountLength/binary>>) ->
    {ok, [ServerId, Account]};

read(10002, <<ServerId:16, AccountLength:16, Account:AccountLength/binary, RoleNameLength:16, RoleName:RoleNameLength/binary, Sex:8, Classes:8, ChannelLength:16, Channel:ChannelLength/binary, DeviceIdLength:16, DeviceId:DeviceIdLength/binary, MacLength:16, Mac:MacLength/binary, DeviceTypeLength:16, DeviceType:DeviceTypeLength/binary>>) ->
    {ok, [ServerId, Account, RoleName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]};

read(10003, <<ServerId:16, AccountLength:16, Account:AccountLength/binary>>) ->
    {ok, [ServerId, Account]};

read(10004, <<ServerId:16, AccountLength:16, Account:AccountLength/binary>>) ->
    {ok, [ServerId, Account]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(10000, []) ->
    {ok, protocol:pack(10000, <<>>)};

write(10001, Result) ->
    {ok, protocol:pack(10001, <<(text(10001, Result))/binary>>)};

write(10002, Result) ->
    {ok, protocol:pack(10002, <<(text(10002, Result))/binary>>)};

write(10003, Result) ->
    {ok, protocol:pack(10003, <<(text(10003, Result))/binary>>)};

write(10004, Result) ->
    {ok, protocol:pack(10004, <<(text(10004, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(10001, no_such_account, sc) ->
    <<15:16, "没有此账户"/utf8>>;
text(10002, duplicate, sc) ->
    <<12:16, "名字重复"/utf8>>;
text(10002, length, sc) ->
    <<12:16, "长度不对"/utf8>>;
text(10002, not_utf8, sc) ->
    <<12:16, "未知字符"/utf8>>;
text(10002, sensitive, sc) ->
    <<15:16, "包含敏感词"/utf8>>;
text(10003, duplicate, sc) ->
    <<12:16, "重复登录"/utf8>>;
text(10003, no_such_name, sc) ->
    <<18:16, "没有此用户名"/utf8>>;
text(10003, permission_denied, sc) ->
    <<12:16, "权限不够"/utf8>>;
text(10003, refuse, sc) ->
    <<12:16, "禁止登录"/utf8>>;
text(10003, server_id_not_match, sc) ->
    <<20:16, "服务器ID不匹配"/utf8>>;
text(10003, server_update, sc) ->
    <<15:16, "服务器更新"/utf8>>;
text(10004, no_such_name, sc) ->
    <<18:16, "没有此用户名"/utf8>>;
text(10004, server_id_not_match, sc) ->
    <<20:16, "服务器ID不匹配"/utf8>>;
text(10004, server_update, sc) ->
    <<15:16, "服务器更新"/utf8>>;
text(_, Reason, _) ->
    protocol:write_bit_string(type:to_binary(Reason)).

