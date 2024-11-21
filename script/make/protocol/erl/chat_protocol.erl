-module(chat_protocol).
-export([decode/2, encode/2]).
-include("chat.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11602, _Rest_ = <<_/binary>>) ->
    <<Data:16, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(11603, _Rest_ = <<_/binary>>) ->
    <<Type:8, _TypeRest_/binary>> = _Rest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, {Type, Message}};

decode(11604, _Rest_ = <<_/binary>>) ->
    <<Data:16, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(11605, _Rest_ = <<_/binary>>) ->
    <<Type:8, _TypeRest_/binary>> = _Rest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, {Type, Message}};

decode(11606, _Rest_ = <<_/binary>>) ->
    <<Data:16, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(11607, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<Type:8, _TypeRest_/binary>> = _RoleIdRest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, {RoleId, Type, Message}};

decode(11608, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<Page:16, _PageRest_/binary>> = _RoleIdRest_,
    {ok, {RoleId, Page}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11602, Data) ->
    Data11602 = <<(encode_data_11602(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11602)):16, 11602:16, Data11602/binary>>};

encode(11603, {Result, #world_chat{id = WorldChatId, role_id = WorldChatRoleId, role_name = WorldChatRoleName, type = WorldChatType, message = WorldChatMessage}}) ->
    Data11603 = <<(protocol:text(Result))/binary, WorldChatId:64, WorldChatRoleId:64, (byte_size(WorldChatRoleName)):16, (WorldChatRoleName)/binary, WorldChatType:8, (byte_size(WorldChatMessage)):16, (WorldChatMessage)/binary>>,
    {ok, <<(byte_size(Data11603)):16, 11603:16, Data11603/binary>>};

encode(11604, Data) ->
    Data11604 = <<(encode_data_11604(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11604)):16, 11604:16, Data11604/binary>>};

encode(11605, {Result, #guild_chat{id = GuildChatId, role_id = GuildChatRoleId, role_name = GuildChatRoleName, type = GuildChatType, message = GuildChatMessage}}) ->
    Data11605 = <<(protocol:text(Result))/binary, GuildChatId:64, GuildChatRoleId:64, (byte_size(GuildChatRoleName)):16, (GuildChatRoleName)/binary, GuildChatType:8, (byte_size(GuildChatMessage)):16, (GuildChatMessage)/binary>>,
    {ok, <<(byte_size(Data11605)):16, 11605:16, Data11605/binary>>};

encode(11606, Data) ->
    Data11606 = <<(encode_data_11606(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11606)):16, 11606:16, Data11606/binary>>};

encode(11607, {Result, #private_chat{sender_id = PrivateChatSenderId, receiver_id = PrivateChatReceiverId, type = PrivateChatType, message = PrivateChatMessage}}) ->
    Data11607 = <<(protocol:text(Result))/binary, PrivateChatSenderId:64, PrivateChatReceiverId:64, PrivateChatType:8, (byte_size(PrivateChatMessage)):16, (PrivateChatMessage)/binary>>,
    {ok, <<(byte_size(Data11607)):16, 11607:16, Data11607/binary>>};

encode(11608, Data) ->
    Data11608 = <<(encode_data_11608(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11608)):16, 11608:16, Data11608/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_11602(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11602(Acc = <<_/binary>>, Length, [#system_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | Data]) ->
    encode_data_11602(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, Data).

encode_data_11604(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11604(Acc = <<_/binary>>, Length, [#world_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | Data]) ->
    encode_data_11604(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, Data).

encode_data_11606(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11606(Acc = <<_/binary>>, Length, [#guild_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | Data]) ->
    encode_data_11606(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, Data).

encode_data_11608(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11608(Acc = <<_/binary>>, Length, [#private_chat{sender_id = SenderId, receiver_id = ReceiverId, type = Type, message = Message} | Data]) ->
    encode_data_11608(<<Acc/binary, SenderId:64, ReceiverId:64, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, Data).

