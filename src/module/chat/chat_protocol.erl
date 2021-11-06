-module(chat_protocol).
-export([read/2, write/2]).
-include("chat.hrl").


read(11602, <<Page:16>>) ->
    {ok, Page};

read(11603, <<Type:8, MessageLength:16, Message:MessageLength/binary>>) ->
    {ok, [Type, Message]};

read(11604, <<Page:16>>) ->
    {ok, Page};

read(11605, <<Type:8, MessageLength:16, Message:MessageLength/binary>>) ->
    {ok, [Type, Message]};

read(11606, <<Page:16>>) ->
    {ok, Page};

read(11607, <<RoleId:64, Type:8, MessageLength:16, Message:MessageLength/binary>>) ->
    {ok, [RoleId, Type, Message]};

read(11608, <<RoleId:64, Page:16>>) ->
    {ok, [RoleId, Page]};

read(Code, Binary) ->
    {error, Code, Binary}.


write(11602, List) ->
    ListBinary = protocol:write_list(fun(#system_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}) -> <<Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>> end, List),
    {ok, protocol:pack(11602, <<ListBinary/binary>>)};

write(11603, [Result, #world_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}]) ->
    {ok, protocol:pack(11603, <<(protocol:text(Result))/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>)};

write(11604, List) ->
    ListBinary = protocol:write_list(fun(#world_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}) -> <<Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>> end, List),
    {ok, protocol:pack(11604, <<ListBinary/binary>>)};

write(11605, [Result, #guild_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}]) ->
    {ok, protocol:pack(11605, <<(protocol:text(Result))/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>)};

write(11606, List) ->
    ListBinary = protocol:write_list(fun(#guild_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}) -> <<Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>> end, List),
    {ok, protocol:pack(11606, <<ListBinary/binary>>)};

write(11607, [Result, #private_chat{sender_id = SenderId, receiver_id = ReceiverId, type = Type, message = Message}]) ->
    {ok, protocol:pack(11607, <<(protocol:text(Result))/binary, SenderId:64, ReceiverId:64, Type:8, (byte_size(Message)):16, (Message)/binary>>)};

write(11608, List) ->
    ListBinary = protocol:write_list(fun(#private_chat{sender_id = SenderId, receiver_id = ReceiverId, type = Type, message = Message}) -> <<SenderId:64, ReceiverId:64, Type:8, (byte_size(Message)):16, (Message)/binary>> end, List),
    {ok, protocol:pack(11608, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


