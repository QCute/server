-module(friend_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("friend.hrl").


read(11501, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11501, [Friend]) ->
    FriendBinary = <<(length(Friend)):16, <<<<FriendId:64, (byte_size(FriendName)):16, (FriendName)/binary, State:8, Time:32>> || #friend{friend_id = FriendId, friend_name = FriendName, state = State, time = Time} <- Friend>>/binary>>,
    {ok, protocol:pack(11501, <<FriendBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.
