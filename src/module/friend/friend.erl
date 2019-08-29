%%%-------------------------------------------------------------------
%%% @doc
%%% module chat
%%% @end
%%%-------------------------------------------------------------------
-module(friend).
%% API
-export([load/1, save/1]).
-export([push/1]).
-export([apply/2, agree/2, delete/2]).
-export([applied/2, agreed/2, deleted/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
-include("friend.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Mails = parser:convert(friend_sql:select(RoleId), ?MODULE),
    User#user{friend = Mails}.

%% @doc save
-spec save(User ::#user{}) -> NewUser :: #user{}.
save(User = #user{friend = Friend}) ->
    NewFriend = friend_sql:update_into(Friend),
    User#user{friend = NewFriend}.

%% @doc push
-spec push(User :: #user{}) -> {reply, list()}.
push(#user{friend = Friend}) ->
    {reply, [Friend]}.

%% @doc apply
-spec apply(User :: #user{}, FriendId :: non_neg_integer()) -> ok() | error().
apply(User = #user{role_id = RoleId, role_name = RoleName, friend = FriendList}, FriendId) ->
    Limit = parameter_data:get(friend_number),
    OpenLevel = parameter_data:get(friend_level),
    case user_manager:lookup(FriendId) of
        [#online{status = Status, level = FriendLevel, role_name = FriendName}] ->
            Check = [{Status, eq, online, [3]}, {level, OpenLevel, [4]}, {OpenLevel, le, FriendLevel, [5]}, {length(FriendList), lt, Limit, [6]}],
            case user_checker:check(User, Check) of
                ok ->
                    %% add self added
                    Self = #friend{role_id = RoleId, friend_id = FriendId, friend_name = FriendName, state = 0, time = time:ts()},
                    friend_sql:insert(Self),
                    %% add the friend side
                    Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = RoleName, state = 0, time = time:ts()},
                    friend_sql:update_into(Friend),
                    %% notify the friend side
                    user_server:apply_cast(Friend, fun applied/2, [Friend]),
                    %% update self side data
                    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Self),
                    user_server:apply_cast(FriendId, fun applied/2, [Friend]),
                    {ok, [1], User#user{friend = NewFriendList}};
                Error ->
                    Error
            end;
        _ ->
            {error, [2]}
    end.

%% apply friend callback
applied(User = #user{friend = FriendList}, Friend = #friend{friend_id = FriendId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_APPLY, [1, Friend]),
    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

-spec agree(User :: #user{}, FriendId :: non_neg_integer()) -> ok() | error().
agree(User = #user{role_id = RoleId, role_name = Name, friend = FriendList}, FriendId) ->
    case lists:keyfind(FriendId, #friend.friend_id, FriendList) of
        SelfFriend = #friend{} ->
            %% add self added
            NewSelfFriend = SelfFriend#friend{state = 1, time = time:ts()},
            friend_sql:update_into([NewSelfFriend]),
            %% add the friend side
            Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = Name, state = 1, time = time:ts()},
            friend_sql:update_into([Friend]),
            %% notify the friend side
            user_server:apply_cast(FriendId, fun agreed/2, [Friend]),
            %% update self side data
            NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, NewSelfFriend),
            {ok, [1], User#user{friend = NewFriendList}};
        _ ->
            {error, [2]}
    end.

%% accept friend side callback
agreed(User = #user{friend = FriendList}, Friend = #friend{friend_id = FriendId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_AGREE, [1, Friend]),
    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

-spec delete(User :: #user{}, FriendId :: non_neg_integer()) -> NewUser :: #user{}.
delete(User = #user{role_id = RoleId, friend = FriendList}, FriendId) ->
    NewFriendList = lists:keydelete(FriendId, #friend.friend_id, FriendList),
    %% delete self
    friend_sql:delete(RoleId, FriendId),
    %% delete the friend side
    friend_sql:delete(FriendId, RoleId),
    %% notify the friend side
    user_server:apply_cast(FriendId, fun deleted/2, [FriendId]),
    {ok, [1, FriendId], User#user{friend = NewFriendList}}.

%% delete friend side callback
deleted(User = #user{friend = FriendList}, FriendId) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_DELETE, [1, FriendId]),
    NewFriendList = lists:keydelete(FriendId, #friend.friend_id, FriendList),
    {ok, User#user{friend = NewFriendList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
