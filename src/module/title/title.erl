%%%-------------------------------------------------------------------
%%% @doc
%%% title
%%% @end
%%%-------------------------------------------------------------------
-module(title).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([expire/1]).
-export([add/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("title.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Title = title_sql:select_by_role_id(RoleId),
    NewUser = lists:foldl(fun(#title{title_id = TitleId}, Acc) -> attribute:add(Acc, {?MODULE, TitleId}, (title_data:get(TitleId))#title_data.attribute) end, User, Title),
    NewUser#user{title = Title}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{title = Title}) ->
    NewTitle = title_sql:insert_update(Title),
    User#user{title = NewTitle}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{title = Title}) ->
    {ok, Title}.

%% @doc expire
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User = #user{title = TitleList}) ->
    Now = time:now(),
    {NewUser, NewList, Delete} = expire_loop(TitleList, User, Now, [], []),
    _ = Delete =/= [] andalso user_sender:send(User, ?PROTOCOL_TITLE_DELETE, Delete) == ok,
    NewUser#user{title = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #title{expire_time = 0} | T], User, Now, List, Delete) ->
    expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #title{role_id = RoleId, title_id = TitleId, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            title_sql:delete(RoleId, TitleId),
            NewUser = user_event:trigger(User, #event{name = event_title_expire, target = TitleId}),
            expire_loop(T, NewUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%% @doc add
-spec add(User :: #user{}, TitleId :: non_neg_integer(), From :: term()) -> ok() | error().
add(User, TitleId, From) ->
    case title_data:get(TitleId) of
        TitleData = #title_data{} ->
            check_duplicate(User, TitleData, From);
        _ ->
            {error, configure_not_found}
    end.

check_duplicate(User = #user{title = TitleList}, TitleData = #title_data{title_id = TitleId}, From) ->
    case lists:keymember(TitleId, #title.title_id, TitleList) of
        false ->
            check_unique(User, TitleData, From);
        true ->
            {error, duplicate_title}
    end.

check_unique(User, TitleData = #title_data{is_unique = false}, From) ->
    check_multi(User, TitleData, From);
check_unique(User = #user{role_id = RoleId}, TitleData = #title_data{title_id = TitleId, is_unique = true}, From) ->
    case title_sql:select_by_title_id(TitleId) of
        [#title{role_id = OtherRoleId} | _] ->
            %% update database
            title_sql:update_role_id(RoleId, OtherRoleId, TitleId),
            %% notify delete
            user_server:apply_cast(OtherRoleId, fun delete/2, [TitleId]),
            %% add to self
            add_new(User, TitleData, From);
        [] ->
            check_multi(User, TitleData, From)
    end.

check_multi(User, TitleData = #title_data{multi = true}, From) ->
    add_new(User, TitleData, From);
check_multi(User = #user{title = TitleList}, TitleData = #title_data{type = Type, multi = false}, From) ->
    case lists:keyfind(Type, #title.type, TitleList) of
        false ->
            add_new(User, TitleData, From);
        #title{} ->
            add_replace(User, TitleData, From)
    end.

add_new(User = #user{title = TitleList}, TitleData = #title_data{title_id = TitleId, type = Type, expire_time = 0}, From) ->
    %% permanent
    NewTitle = #title{title_id = TitleId, type = Type, expire_time = 0},
    add_final(User#user{title = [NewTitle |  TitleList]}, NewTitle, TitleData, From);
add_new(User = #user{title = TitleList}, TitleData = #title_data{title_id = TitleId, type = Type, expire_time = ExpireTime}, From) ->
    %% with expire time
    NewTitle = #title{title_id = TitleId, type = Type, expire_time = time:now() + ExpireTime},
    add_final(User#user{title = [NewTitle |  TitleList]}, NewTitle, TitleData, From).

add_replace(User = #user{role_id = RoleId, title = TitleList}, TitleData = #title_data{title_id = TitleId, type = Type, expire_time = 0}, From) ->
    %% permanent
    NewTitle = #title{role_id = RoleId, title_id = TitleId, type = Type, expire_time = 0},
    %% replace same type title
    NewTitleList = lists:keyreplace(Type, #title.type, TitleList, NewTitle),
    add_final(User#user{title = NewTitleList}, NewTitle, TitleData, From);
add_replace(User = #user{role_id = RoleId, title = TitleList}, TitleData = #title_data{title_id = TitleId, type = Type, expire_time = ExpireTime}, From) ->
    %% with expire time
    NewTitle = #title{role_id = RoleId, title_id = TitleId, type = Type, expire_time = time:now() + ExpireTime},
    %% replace same type title
    NewTitleList = lists:keyreplace(Type, #title.type, TitleList, NewTitle),
    add_final(User#user{title = NewTitleList}, NewTitle, TitleData, From).

add_final(User = #user{role_id = RoleId}, #title{title_id = TitleId}, #title_data{attribute = Attribute}, From) ->
    %% calculate attribute
    NewUser = attribute:recalculate(User, {?MODULE, TitleId}, Attribute),
    %% handle add title event
    FinalUser = user_event:trigger(NewUser, #event{name = event_title_add, target = TitleId}),
    log:title_log(RoleId, TitleId, From, time:now()),
    {ok, FinalUser}.

%% async delete callback
delete(User = #user{title = TitleList}, TitleId) ->
    case lists:keytake(TitleId, #title.title_id, TitleList) of
        {value, Title, NewTitleList} ->
            user_sender:send(User, ?PROTOCOL_TITLE_DELETE, [Title]),
            NewUser = attribute:recalculate(User, {?MODULE, TitleId}, []),
            {ok, NewUser#user{title = NewTitleList}};
        _ ->
            {error, no_such_title}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
