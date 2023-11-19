-module(chat_protocol).
-export([decode/2, encode/2]).
-include("chat.hrl").


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11602, _Rest_ = <<_/binary>>) ->
    <<Page:16, _PageRest_/binary>> = _Rest_,
    {ok, Page};

decode(11603, _Rest_ = <<_/binary>>) ->
    <<Type:8, _TypeRest_/binary>> = _Rest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, [Type, Message]};

decode(11604, _Rest_ = <<_/binary>>) ->
    <<Page:16, _PageRest_/binary>> = _Rest_,
    {ok, Page};

decode(11605, _Rest_ = <<_/binary>>) ->
    <<Type:8, _TypeRest_/binary>> = _Rest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, [Type, Message]};

decode(11606, _Rest_ = <<_/binary>>) ->
    <<Page:16, _PageRest_/binary>> = _Rest_,
    {ok, Page};

decode(11607, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<Type:8, _TypeRest_/binary>> = _RoleIdRest_,
    <<MessageLength:16, Message:MessageLength/binary, _MessageRest_/binary>> = _TypeRest_,
    {ok, [RoleId, Type, Message]};

decode(11608, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<Page:16, _PageRest_/binary>> = _RoleIdRest_,
    {ok, [RoleId, Page]};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11602, List) ->
    Data11602 = <<(encode_list_11602(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11602)):16, 11602:16, Data11602/binary>>};

encode(11603, [Result, #world_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}]) ->
    Data11603 = <<(protocol:text(Result))/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>,
    {ok, <<(byte_size(Data11603)):16, 11603:16, Data11603/binary>>};

encode(11604, List) ->
    Data11604 = <<(encode_list_11604(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11604)):16, 11604:16, Data11604/binary>>};

encode(11605, [Result, #guild_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message}]) ->
    Data11605 = <<(protocol:text(Result))/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>,
    {ok, <<(byte_size(Data11605)):16, 11605:16, Data11605/binary>>};

encode(11606, List) ->
    Data11606 = <<(encode_list_11606(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11606)):16, 11606:16, Data11606/binary>>};

encode(11607, [Result, #private_chat{sender_id = SenderId, receiver_id = ReceiverId, type = Type, message = Message}]) ->
    Data11607 = <<(protocol:text(Result))/binary, SenderId:64, ReceiverId:64, Type:8, (byte_size(Message)):16, (Message)/binary>>,
    {ok, <<(byte_size(Data11607)):16, 11607:16, Data11607/binary>>};

encode(11608, List) ->
    Data11608 = <<(encode_list_11608(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11608)):16, 11608:16, Data11608/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_11602(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11602(Acc = <<_/binary>>, Length, [#system_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | List]) ->
    encode_list_11602(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, List).

encode_list_11604(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11604(Acc = <<_/binary>>, Length, [#world_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | List]) ->
    encode_list_11604(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, List).

encode_list_11606(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11606(Acc = <<_/binary>>, Length, [#guild_chat{id = Id, role_id = RoleId, role_name = RoleName, type = Type, message = Message} | List]) ->
    encode_list_11606(<<Acc/binary, Id:64, RoleId:64, (byte_size(RoleName)):16, (RoleName)/binary, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, List).

encode_list_11608(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11608(Acc = <<_/binary>>, Length, [#private_chat{sender_id = SenderId, receiver_id = ReceiverId, type = Type, message = Message} | List]) ->
    encode_list_11608(<<Acc/binary, SenderId:64, ReceiverId:64, Type:8, (byte_size(Message)):16, (Message)/binary>>, Length + 1, List).

