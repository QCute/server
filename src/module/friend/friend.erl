%%%-------------------------------------------------------------------
%%% @doc
%%% module chat
%%% @end
%%%-------------------------------------------------------------------
-module(friend).
%% API
-export([load/1, save/1]).
-export([apply/2, accept/3, delete/2]).
-export([applied/2, agreed/2, deleted/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
-include("role.hrl").
-include("friend.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Data = friend_sql:select(RoleId),
    Mails = parser:convert(Data, friend),
    User#user{friend = Mails}.

%% @doc save
-spec save(User ::#user{}) -> NewUser :: #user{}.
save(User = #user{friend = Friend}) ->
    NewFriend = friend_sql:update_into(Friend),
    User#user{friend = NewFriend}.

%% @doc apply
-spec apply(User :: #user{}, FriendId :: non_neg_integer()) -> ok() | error().
apply(User = #user{role_id = RoleId, role_name = RoleName, friend = FriendList}, FriendId) ->
    Limit = parameter_data:get(friend_number),
    OpenLevel = parameter_data:get(friend_level),
    case user_manager:lookup(FriendId) of
        [#online{status = Status, level = FriendLevel, role_name = FriendName}] ->
            Check = [{Status, eq, online, 3}, {level, OpenLevel, 4}, {OpenLevel, le, FriendLevel, 5}, {length(FriendList), lt, Limit, 6}],
            case user_checker:check(User, Check) of
                ok ->
                    %% add self added
                    Self = #friend{role_id = RoleId, friend_id = FriendId, friend_name = FriendName, state = 1, time = time:ts()},
                    friend_sql:insert(Self),
                    %% add the friend side
                    Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = RoleName, state = 1, time = time:ts()},
                    friend_sql:update_into(Friend),
                    %% notify the friend side
                    user_server:apply_cast(Friend, ?MODULE, accepted, [Friend]),
                    %% update self side data
                    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Self),
                    user_server:apply_cast(FriendId, ?MODULE, applied, [Friend]),
                    {ok, Self, User#user{friend = NewFriendList}};
                Error ->
                    Error
            end;
        _ ->
            {error, 2}
    end.

%% @doc apply friend callback
-spec applied(User :: #user{}, Friend :: #friend{}) -> NewUser :: #user{}.
applied(User = #user{friend = FriendList}, Friend = #friend{friend_id = FriendId}) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_APPLY, [1, Friend]),
    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Friend),
    {ok, User#user{friend = NewFriendList}}.

-spec accept(User :: #user{}, FriendId :: non_neg_integer(), FriendName :: binary()) -> ok() | error().
accept(User = #user{role_id = RoleId, role_name = Name, friend = FriendList}, FriendId, FriendName) ->
    %% add self added
    Self = #friend{role_id = RoleId, friend_id = FriendId, friend_name = FriendName, state = 1, time = time:ts()},
    friend_sql:update_into([Self]),
    %% add the friend side
    Friend = #friend{role_id = FriendId, friend_id = RoleId, friend_name = Name, state = 1, time = time:ts()},
    friend_sql:update_into([Friend]),
    %% notify the friend side
    user_server:apply_cast(Friend, ?MODULE, agreed, [Friend]),
    %% update self side data
    NewFriendList = lists:keystore(FriendId, #friend.friend_id, FriendList, Self),
    {ok, Self, User#user{friend = NewFriendList}}.

%% @doc accept friend side callback
-spec agreed(User :: #user{}, Friend :: #friend{}) -> NewUser :: #user{}.
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
    user_server:apply_cast(FriendId, ?MODULE, deleted, [FriendId]),
    {ok, User#user{friend = NewFriendList}}.

%% @doc delete friend side callback
-spec deleted(User :: #user{}, Friend :: non_neg_integer()) -> NewUser :: #user{}.
deleted(User = #user{friend = FriendList}, FriendId) ->
    %% @todo notify client
    user_sender:send(User, ?PROTOCOL_FRIEND_DELETE, [1, FriendId]),
    NewFriendList = lists:keydelete(FriendId, #friend.friend_id, FriendList),
    {ok, User#user{friend = NewFriendList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
