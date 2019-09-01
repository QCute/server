-module(friend_handler).
-export([handle/3]).

handle(11501, User, []) ->
    friend:query(User);

handle(11502, User, [FriendId]) ->
    friend:apply(User, FriendId);

handle(11503, User, [FriendId]) ->
    friend:agree(User, FriendId);

handle(11504, User, [FriendId]) ->
    friend:delete(User, FriendId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
