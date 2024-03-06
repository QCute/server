-module(friend_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_apply/2]).
-export([send_agree/2]).
-export([send_delete/3]).
-export([send_block/3]).
-export([send_cancel_block/3]).
-include("user.hrl").

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

send_query(User, List) ->
    {ok, Binary} = friend_protocol:encode(11501, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_apply(User, Result) ->
    {ok, Binary} = friend_protocol:encode(11502, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_agree(User, Result) ->
    {ok, Binary} = friend_protocol:encode(11503, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_delete(User, Result, FriendRoleId) ->
    {ok, Binary} = friend_protocol:encode(11504, [Result, FriendRoleId]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_block(User, Result, FriendRoleId) ->
    {ok, Binary} = friend_protocol:encode(11505, [Result, FriendRoleId]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_cancel_block(User, Result, FriendRoleId) ->
    {ok, Binary} = friend_protocol:encode(11506, [Result, FriendRoleId]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

