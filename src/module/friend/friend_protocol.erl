-module(friend_protocol).
-export([read/2, write/2]).
-include("friend.hrl").


read(11501, <<>>) ->
    {ok, []};

read(11502, <<FriendId:64>>) ->
    {ok, FriendId};

read(11503, <<FriendId:64>>) ->
    {ok, FriendId};

read(11504, <<FriendId:64>>) ->
    {ok, FriendId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11501, Friend) ->
    {ok, protocol:pack(11501, <<(length(Friend)):16, <<<<FriendId:64, (byte_size(FriendName)):16, (FriendName)/binary, Relation:8, Time:32>> || #friend{friend_id = FriendId, friend_name = FriendName, relation = Relation, time = Time} <- Friend>>/binary>>)};

write(11502, Result) ->
    {ok, protocol:pack(11502, <<Result:8>>)};

write(11503, Result) ->
    {ok, protocol:pack(11503, <<Result:8>>)};

write(11504, [Result, FriendId]) ->
    {ok, protocol:pack(11504, <<Result:8, FriendId:64>>)};

write(Code, Content) ->
    {error, Code, Content}.
