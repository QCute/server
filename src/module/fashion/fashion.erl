%%%-------------------------------------------------------------------
%%% @doc
%%% fashion
%%% @end
%%%-------------------------------------------------------------------
-module(fashion).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([expire/1]).
-export([add/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("fashion.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Fashion = fashion_sql:select_by_role_id(RoleId),
    User#user{fashion = Fashion}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{fashion = Fashion}) ->
    NewFashion = fashion_sql:insert_update(Fashion),
    User#user{fashion = NewFashion}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{fashion = Fashion}) ->
    {ok, Fashion}.

%% @doc expire
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User = #user{fashion = FashionList}) ->
    Now = time:now(),
    {NewUser, NewList, Delete} = expire_loop(FashionList, User, Now, [], []),
    _ = Delete =/= [] andalso user_sender:send(User, ?PROTOCOL_FASHION_DELETE, Delete) == ok,
    NewUser#user{fashion = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #fashion{expire_time = 0} | T], User, Now, List, Delete) ->
    expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #fashion{role_id = RoleId, fashion_id = FashionId, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            fashion_sql:delete(RoleId, FashionId),
            NewUser = user_event:trigger(User, #event{name = event_fashion_expire, target = FashionId}),
            expire_loop(T, NewUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%% @doc add
-spec add(User :: #user{}, FashionId :: non_neg_integer(), From :: term()) -> ok() | error().
add(User, FashionId, From) ->
    case fashion_data:get(FashionId) of
        FashionData = #fashion_data{} ->
            check_duplicate(User, FashionData, From);
        _ ->
            {error, configure_not_found}
    end.

check_duplicate(User = #user{fashion = FashionList}, FashionData = #fashion_data{fashion_id = FashionId}, From) ->
    case lists:keymember(FashionId, #fashion.fashion_id, FashionList) of
        false ->
            check_unique(User, FashionData, From);
        true ->
            {error, fashion_duplicated}
    end.

check_unique(User, FashionData = #fashion_data{is_unique = false}, From) ->
    add_new(User, FashionData, From);
check_unique(User = #user{role_id = RoleId}, FashionData = #fashion_data{fashion_id = FashionId, is_unique = true}, From) ->
    case fashion_sql:select_by_fashion_id(FashionId) of
        [#fashion{role_id = OtherRoleId} | _] ->
            %% update database
            fashion_sql:update_role_id(RoleId, OtherRoleId, FashionId),
            %% notify delete
            user_server:apply_cast(OtherRoleId, fun delete/2, [FashionId]),
            %% add to self
            add_new(User, FashionData, From);
        [] ->
            add_new(User, FashionData, From)
    end.

add_new(User = #user{fashion = FashionList}, FashionData = #fashion_data{fashion_id = FashionId, type = Type, expire_time = 0}, From) ->
    %% permanent
    NewFashion = #fashion{fashion_id = FashionId, type = Type, expire_time = 0},
    add_final(User#user{fashion = [NewFashion |  FashionList]}, NewFashion, FashionData, From);
add_new(User = #user{fashion = FashionList}, FashionData = #fashion_data{fashion_id = FashionId, type = Type, expire_time = ExpireTime}, From) ->
    %% with expire time
    NewFashion = #fashion{fashion_id = FashionId, type = Type, expire_time = time:now() + ExpireTime},
    add_final(User#user{fashion = [NewFashion |  FashionList]}, NewFashion, FashionData, From).

add_final(User = #user{role_id = RoleId}, #fashion{fashion_id = FashionId}, #fashion_data{attribute = Attribute}, From) ->
    %% calculate attribute
    NewUser = attribute:recalculate(User, {?MODULE, FashionId}, Attribute),
    %% handle add fashion event
    FinalUser = user_event:trigger(NewUser, #event{name = event_fashion_add, target = FashionId}),
    log:fashion_log(RoleId, FashionId, From, time:now()),
    {ok, FinalUser}.

%% async delete callback
delete(User = #user{fashion = FashionList}, FashionId) ->
    case lists:keytake(FashionId, #fashion.fashion_id, FashionList) of
        {value, Fashion, NewFashionList} ->
            user_sender:send(User, ?PROTOCOL_FASHION_DELETE, [Fashion]),
            NewUser = attribute:recalculate(User, {?MODULE, FashionId}, []),
            {ok, NewUser#user{fashion = NewFashionList}};
        _ ->
            {error, fashion_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

