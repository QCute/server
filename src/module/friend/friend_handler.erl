%%%-------------------------------------------------------------------
%%% @doc
%%% module chat handle
%%% @end
%%%-------------------------------------------------------------------
-module(friend_handler).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("friend.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?CMD_FRIEND, #user{friend = Friend}, []) ->
    {reply, Friend};

%% @doc apply friend
handle(?CMD_FRIEND_APPLY, User, [FriendId]) ->
    case friend:apply(User, FriendId) of
        {ok, Friend, NewUser} ->
            {reply, [1, Friend], NewUser};
        {error, Code} ->
            {reply, [Code, #friend{}]}
    end;

%% @doc accept friend
handle(?CMD_FRIEND_AGREE, User, [FriendId, FriendName]) ->
    case friend:accept(User, FriendId, FriendName) of
        {ok, Friend, NewUser} ->
            {reply, [1, Friend], NewUser};
        {error, Code} ->
            {reply, [Code, #friend{}]}
    end;

%% @doc delete friend
handle(?CMD_FRIEND_DELETE, User, [FriendId]) ->
    case friend:delete(User, FriendId) of
        {ok, NewUser} ->
            {reply, [1, FriendId], NewUser};
        {error, Code} ->
            {reply, [Code, FriendId]}
    end;

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


