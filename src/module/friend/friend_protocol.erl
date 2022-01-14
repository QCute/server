-module(friend_protocol).
-export([read/2, write/2]).
-include("friend.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11501, <<>>) ->
    {ok, []};

read(11502, <<FriendRoleId:64>>) ->
    {ok, FriendRoleId};

read(11503, <<FriendRoleId:64>>) ->
    {ok, FriendRoleId};

read(11504, <<FriendRoleId:64>>) ->
    {ok, FriendRoleId};

read(11505, <<FriendRoleId:64>>) ->
    {ok, FriendRoleId};

read(11506, <<FriendRoleId:64>>) ->
    {ok, FriendRoleId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11501, List) ->
    ListBinary = protocol:write_list(fun(#friend{friend_role_id = FriendRoleId, friend_name = FriendName, relation = Relation, time = Time}) -> <<FriendRoleId:64, (byte_size(FriendName)):16, (FriendName)/binary, Relation:8, Time:32>> end, List),
    {ok, protocol:pack(11501, <<ListBinary/binary>>)};

write(11502, Result) ->
    {ok, protocol:pack(11502, <<(protocol:text(Result))/binary>>)};

write(11503, Result) ->
    {ok, protocol:pack(11503, <<(protocol:text(Result))/binary>>)};

write(11504, [Result, FriendRoleId]) ->
    {ok, protocol:pack(11504, <<(protocol:text(Result))/binary, FriendRoleId:64>>)};

write(11505, [Result, FriendRoleId]) ->
    {ok, protocol:pack(11505, <<(protocol:text(Result))/binary, FriendRoleId:64>>)};

write(11506, [Result, FriendRoleId]) ->
    {ok, protocol:pack(11506, <<(protocol:text(Result))/binary, FriendRoleId:64>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


