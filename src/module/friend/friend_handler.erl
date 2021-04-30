-module(friend_handler).
-export([handle/3]).

handle(11501, User, []) ->
    friend:query(User);

handle(11502, User, FriendRoleId) ->
    friend:apply(User, FriendRoleId);

handle(11503, User, FriendRoleId) ->
    friend:agree(User, FriendRoleId);

handle(11504, User, FriendRoleId) ->
    friend:delete(User, FriendRoleId);

handle(11505, User, FriendRoleId) ->
    friend:block(User, FriendRoleId);

handle(11506, User, FriendRoleId) ->
    friend:cancel_block(User, FriendRoleId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
