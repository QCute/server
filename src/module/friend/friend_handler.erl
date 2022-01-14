-module(friend_handler).
-export([handle/3]).

handle(User, 11501, []) ->
    friend:query(User);

handle(User, 11502, FriendRoleId) ->
    friend:apply(User, FriendRoleId);

handle(User, 11503, FriendRoleId) ->
    friend:agree(User, FriendRoleId);

handle(User, 11504, FriendRoleId) ->
    friend:delete(User, FriendRoleId);

handle(User, 11505, FriendRoleId) ->
    friend:block(User, FriendRoleId);

handle(User, 11506, FriendRoleId) ->
    friend:cancel_block(User, FriendRoleId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
