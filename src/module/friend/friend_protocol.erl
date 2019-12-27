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
    {ok, protocol:pack(11502, <<(text(11502, Result))/binary>>)};

write(11503, Result) ->
    {ok, protocol:pack(11503, <<(text(11503, Result))/binary>>)};

write(11504, [Result, FriendId]) ->
    {ok, protocol:pack(11504, <<(text(11504, Result))/binary, FriendId:64>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(11502, friend_level_not_enough) ->
    <<21:16, "对方好友未开放"/utf8>>;
text(11502, friend_number_max) ->
    <<24:16, "好友数量达到上限"/utf8>>;
text(11502, level_not_enough) ->
    <<15:16, "好友未开放"/utf8>>;
text(11502, user_offline) ->
    <<15:16, "对方不在线"/utf8>>;
text(11503, no_such_apply) ->
    <<24:16, "没有此好友的申请"/utf8>>;
text(_, 0) ->
    <<0:16>>;
text(_, ok) ->
    <<0:16>>;
text(_, Reason) ->
    <<(protocol:write_bit_string(type:to_binary(Reason)))/binary>>.

