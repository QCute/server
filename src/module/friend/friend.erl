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
-export([apply/2, agree/2, delete/2]).
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
-spec apply(User :: #user{}, FriendId :: non_neg_integer()) -> ok() | error().
apply(User = #user{role_id = RoleId, role_name = RoleName, friend = FriendList}, FriendId) ->
    Limit = parameter_data:get(friend_number),
    OpenLevel = parameter_data:get(friend_level),
    case user_manager:lookup(FriendId) of
        [#online{status = Status, level = FriendLevel, role_name = FriendName}] ->
            Check = [{Status, eq, online, user_offline}, {level, OpenLevel, level_not_enough}, {OpenLevel, le, FriendLevel, friend_level_not_enough}, {length(FriendList), lt, Limit, friend_number_max}],
            case user_checker:check(User, Check) of
                ok ->
                    %% add self added
                    Self = #friend{role_id = RoleId, friend_id = FriendId, friend_name = FriendName, relation = 0, time = time:now()},
                    friend_sql:insert_update([Self]),
                    %% add the friend side
                    Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = RoleName, relation = 0, time = time:now()},
                    friend_sql:insert_update([Friend]),
                    %% notify the friend side
                    user_server:apply_cast(FriendId, fun applied/2, [Friend]),
                    %% update self side data
                    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Self),
                    user_server:apply_cast(FriendId, fun applied/2, [Friend]),
                    {ok, ok, User#user{friend = NewFriendList}};
                Error ->
                    Error
            end;
        _ ->
            {error, user_offline}
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
        SelfFriend = #friend{relation = 0} ->
            %% add self added
            NewSelfFriend = SelfFriend#friend{relation = 1, time = time:now()},
            friend_sql:insert_update([NewSelfFriend]),
            %% add the friend side
            Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = Name, relation = 1, time = time:now()},
            friend_sql:insert_update([Friend]),
            %% notify the friend side
            user_server:apply_cast(FriendId, fun agreed/2, [Friend]),
            %% update self side data
            NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, NewSelfFriend),
            {ok, ok, User#user{friend = NewFriendList}};
        _ ->
            {error, no_such_apply}
    end.

%% accept friend side callback
agreed(User = #user{friend = FriendList}, Friend = #friend{friend_id = FriendId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_AGREE, [1, Friend]),
    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

-spec delete(User :: #user{}, FriendId :: non_neg_integer()) -> ok() | error().
delete(User = #user{role_id = RoleId, friend = FriendList}, FriendId) ->
    NewFriendList = lists:keydelete(FriendId, #friend.friend_id, FriendList),
    %% delete self
    friend_sql:delete(RoleId, FriendId),
    %% delete the friend side
    friend_sql:delete(FriendId, RoleId),
    %% notify the friend side
    user_server:apply_cast(FriendId, fun deleted/2, [FriendId]),
    {ok, [ok, FriendId], User#user{friend = NewFriendList}}.

%% delete friend side callback
deleted(User = #user{friend = FriendList}, FriendId) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_DELETE, [1, FriendId]),
    NewFriendList = lists:keydelete(FriendId, #friend.friend_id, FriendList),
    {ok, User#user{friend = NewFriendList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
