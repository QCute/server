%%%-------------------------------------------------------------------
%%% @doc
%%% friend
%%% @end
%%%-------------------------------------------------------------------
-module(friend).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([get_number/1]).
-export([apply/2, agree/2, delete/2, block/2, cancel_block/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("online.hrl").
-include("event.hrl").
-include("friend.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Friend = friend_sql:select_join_by_role_id(RoleId),
    User#user{friend = Friend}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{friend = Friend}) ->
    NewFriend = friend_sql:insert_update(Friend),
    User#user{friend = NewFriend}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{friend = Friend}) ->
    {ok, Friend}.

%% @doc get number
-spec get_number(User :: #user{}) -> non_neg_integer().
get_number(#user{friend = Friend}) ->
    length(Friend).

%% @doc apply
-spec apply(User :: #user{}, FriendRoleId :: non_neg_integer()) -> ok() | error().
apply(User = #user{friend = FriendList}, FriendRoleId) ->
    Limit = parameter_data:get(friend_number),
    OpenLevel = parameter_data:get(friend_level),
    case user_checker:check(User, [{level, OpenLevel, level_not_met}, {length(FriendList), lt, Limit, friend_number_max}]) of
        ok ->
            apply_check(User, FriendRoleId);
        Error ->
            Error
    end.

apply_check(User = #user{friend = FriendList}, FriendRoleId) ->
    case lists:keyfind(FriendRoleId, #friend.friend_role_id, FriendList) of
        false ->
            apply_check_friend(User, FriendRoleId);
        #friend{relation = ?FRIEND_RELATION_APPLY} ->
            {error, friend_in_apply};
        #friend{relation = ?FRIEND_RELATION_FRIEND} ->
            {error, friend_in_list};
        #friend{relation = ?FRIEND_RELATION_BLOCK} ->
            {error, friend_in_block};
        #friend{relation = ?FRIEND_RELATION_BE_BLOCK} ->
            {error, friend_in_be_block}
    end.

apply_check_friend(User, FriendRoleId) ->
    OpenLevel = parameter_data:get(friend_level),
    case user_manager:lookup(FriendRoleId) of
        [Online = #online{level = FriendLevel}] when FriendLevel =< OpenLevel->
            apply_update(User, Online);
        [_] ->
            {error, friend_level_not_met};
        _ ->
            {error, user_offline}
    end.

apply_update(User = #user{role_id = RoleId, friend = FriendList}, Online = #online{role_id = FriendRoleId}) ->
    %% add self added
    Self = user_convert:to_self_friend(User, Online),
    [NewSelf] = friend_sql:insert_update([Self]),
    %% add the friend side
    Friend = user_convert:to_friend(User, RoleId),
    [NewFriend] = friend_sql:insert_update([Friend]),
    %% notify the friend side
    user_server:apply_cast(FriendRoleId, fun applied/2, [NewFriend]),
    %% update self side data
    NewFriendList = lists:keystore(FriendRoleId, #friend.friend_role_id, FriendList, NewSelf),
    {ok, ok, User#user{friend = NewFriendList}}.

%% apply friend callback
applied(User = #user{friend = FriendList}, Friend = #friend{friend_role_id = FriendRoleId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_APPLY, [1, Friend]),
    NewFriendList = lists:keystore(FriendRoleId, #friend.friend_role_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

%% @doc agree
-spec agree(User :: #user{}, FriendRoleId :: non_neg_integer()) -> ok() | error().
agree(User = #user{friend = FriendList}, FriendRoleId) ->
    Limit = parameter_data:get(friend_number),
    case length(FriendList) < Limit of
        true ->
            agree_update(User, FriendRoleId);
        false ->
            {error, friend_number_max}
    end.

agree_update(User = #user{role_id = RoleId, role_name = Name, friend = FriendList}, FriendRoleId) ->
    case lists:keyfind(FriendRoleId, #friend.friend_role_id, FriendList) of
        SelfFriend = #friend{relation = ?FRIEND_RELATION_APPLY} ->
            %% add self added
            NewSelfFriend = SelfFriend#friend{relation = ?FRIEND_RELATION_FRIEND, time = time:now(), flag = 1},
            [NewestSelfFriend] = friend_sql:insert_update([NewSelfFriend]),
            %% add the friend side
            Friend = #friend{role_id = FriendRoleId, friend_role_id = RoleId, friend_name = Name, relation = ?FRIEND_RELATION_FRIEND, time = time:now(), flag = 1},
            [NewFriend] = friend_sql:insert_update([Friend]),
            %% notify the friend side
            user_server:apply_cast(FriendRoleId, fun agreed/2, [NewFriend]),
            %% update self side data
            NewFriendList = lists:keystore(FriendRoleId, #friend.friend_role_id, FriendList, NewestSelfFriend),
            {ok, ok, User#user{friend = NewFriendList}};
        _ ->
            {error, friend_apply_not_found}
    end.

%% accept friend side callback
agreed(User = #user{friend = FriendList}, Friend = #friend{friend_role_id = FriendRoleId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_AGREE, [1, Friend]),
    NewFriendList = lists:keystore(FriendRoleId, #friend.friend_role_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

%% @doc delete
-spec delete(User :: #user{}, FriendRoleId :: non_neg_integer()) -> ok() | error().
delete(User = #user{role_id = RoleId, friend = FriendList}, FriendRoleId) ->
    NewFriendList = lists:keydelete(FriendRoleId, #friend.friend_role_id, FriendList),
    %% delete self side
    friend_sql:delete(RoleId, FriendRoleId),
    %% delete the friend side
    friend_sql:delete(FriendRoleId, RoleId),
    %% notify the friend side
    user_server:apply_cast(FriendRoleId, fun deleted/2, [RoleId]),
    {ok, [ok, FriendRoleId], User#user{friend = NewFriendList}}.

%% delete friend side callback
deleted(User = #user{role_id = RoleId, friend = FriendList}, FriendRoleId) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_DELETE, [1, FriendRoleId]),
    NewFriendList = lists:keydelete(FriendRoleId, #friend.friend_role_id, FriendList),
    %% delete friend side again
    friend_sql:delete(RoleId, FriendRoleId),
    {ok, User#user{friend = NewFriendList}}.

%% @doc block
-spec block(User :: #user{}, FriendRoleId :: non_neg_integer()) -> ok() | error().
block(User = #user{friend = FriendList}, FriendRoleId) ->
    case lists:keyfind(FriendRoleId, #friend.friend_role_id, FriendList) of
        Friend = #friend{relation = ?FRIEND_RELATION_FRIEND} ->
            block_update(User, Friend);
        #friend{relation = ?FRIEND_RELATION_APPLY} ->
            {error, friend_in_apply};
        #friend{relation = ?FRIEND_RELATION_BLOCK} ->
            {error, friend_in_block};
        #friend{relation = ?FRIEND_RELATION_BE_BLOCK} ->
            {error, friend_in_be_block};
        false ->
            {error, friend_not_found}
    end.

block_update(User = #user{role_id = RoleId, friend = FriendList}, Friend = #friend{friend_role_id = FriendRoleId}) ->
    NewFriend = Friend#friend{relation = ?FRIEND_RELATION_BLOCK},
    NewFriendList = lists:keystore(FriendRoleId, #friend.friend_role_id, FriendList, NewFriend),
    %% update self side
    friend_sql:update_relation(RoleId, FriendRoleId, ?FRIEND_RELATION_BLOCK),
    %% update the friend side
    friend_sql:update_relation(FriendRoleId, RoleId, ?FRIEND_RELATION_BE_BLOCK),
    %% notify the friend side
    user_server:apply_cast(FriendRoleId, fun blocked/2, [RoleId]),
    {ok, ok, User#user{friend = NewFriendList}}.

%% delete friend side callback
blocked(User = #user{role_id = RoleId, friend = FriendList}, FriendRoleId) ->
    %% @todo notify client
    Friend = listing:key_find(FriendRoleId, #friend.friend_role_id, FriendList, #friend{}),
    NewFriend = Friend#friend{relation = ?FRIEND_RELATION_BE_BLOCK},
    NewFriendList = lists:keyreplace(FriendRoleId, #friend.friend_role_id, FriendList, NewFriend),
    %% update the friend side again
    friend_sql:update_relation(FriendRoleId, RoleId, ?FRIEND_RELATION_BE_BLOCK),
    {ok, User#user{friend = NewFriendList}}.

%% @doc cancel block
-spec cancel_block(User :: #user{}, FriendRoleId :: non_neg_integer()) -> ok() | error().
cancel_block(User = #user{friend = FriendList}, FriendRoleId) ->
    case lists:keyfind(FriendRoleId, #friend.friend_role_id, FriendList) of
        #friend{relation = ?FRIEND_RELATION_BLOCK} ->
            cancel_block_update(User, FriendRoleId);
        #friend{relation = ?FRIEND_RELATION_APPLY} ->
            {error, friend_in_apply};
        #friend{relation = ?FRIEND_RELATION_FRIEND} ->
            {error, friend_in_list};
        #friend{relation = ?FRIEND_RELATION_BE_BLOCK} ->
            {error, friend_in_be_block};
        false ->
            {error, friend_not_found}
    end.

cancel_block_update(User = #user{role_id = RoleId, friend = FriendList}, FriendRoleId) ->
    NewFriendList = lists:keydelete(FriendRoleId, #friend.friend_role_id, FriendList),
    %% delete self side
    friend_sql:delete(RoleId, FriendRoleId),
    %% delete the friend side
    friend_sql:delete(FriendRoleId, RoleId),
    %% notify the friend side
    user_server:apply_cast(FriendRoleId, fun cancel_blocked/2, [RoleId]),
    {ok, [ok, FriendRoleId], User#user{friend = NewFriendList}}.

%% delete friend side callback
cancel_blocked(User = #user{role_id = RoleId, friend = FriendList}, FriendRoleId) ->
    %% @todo notify client
    NewFriendList = lists:keydelete(FriendRoleId, #friend.friend_role_id, FriendList),
    %% delete friend side again
    friend_sql:delete(RoleId, FriendRoleId),
    {ok, User#user{friend = NewFriendList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
