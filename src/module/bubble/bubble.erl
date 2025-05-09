%%%-------------------------------------------------------------------
%%% @doc
%%% bubble
%%% @end
%%%-------------------------------------------------------------------
-module(bubble).
%% API
-export([on_load/1, on_save/1, on_expire/1]).
-export([query/1]).
-export([add/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("bubble.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Bubble = bubble_sql:select(RoleId),
    User#user{bubble = Bubble}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{bubble = Bubble}) ->
    NewBubble = bubble_sql:save(Bubble),
    User#user{bubble = NewBubble}.

%% @doc on expire
-spec on_expire(User :: #user{}) -> NewUser :: #user{}.
on_expire(User = #user{bubble = BubbleList}) ->
    Now = time:now(),
    {NewUser, NewList, Delete} = expire_loop(BubbleList, User, Now, [], []),
    _ = Delete =/= [] andalso user_sender:send(User, ?PROTOCOL_BUBBLE_DELETE, Delete) == ok,
    NewUser#user{bubble = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #bubble{expire_time = 0} | T], User, Now, List, Delete) ->
    expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #bubble{role_id = RoleId, bubble_id = BubbleId, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            bubble_sql:delete(RoleId, BubbleId),
            NewUser = event:trigger(User, #event{name = bubble_expire, target = BubbleId}),
            expire_loop(T, NewUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{bubble = Bubble}) ->
    {ok, Bubble}.

%% @doc add
-spec add(User :: #user{}, BubbleId :: non_neg_integer(), From :: term()) -> ok() | error().
add(User, BubbleId, From) ->
    case bubble_data:get(BubbleId) of
        BubbleData = #bubble_data{} ->
            check_duplicate(User, BubbleData, From);
        _ ->
            {error, configure_not_found}
    end.

check_duplicate(User = #user{bubble = BubbleList}, BubbleData = #bubble_data{bubble_id = BubbleId}, From) ->
    case lists:keymember(BubbleId, #bubble.bubble_id, BubbleList) of
        false ->
            add_final(User, BubbleData, From);
        true ->
            {error, bubble_duplicated}
    end.

add_final(User = #user{role_id = RoleId}, #bubble_data{bubble_id = BubbleId}, From) ->
    %% handle add bubble event
    FinalUser = event:trigger(User, #event{name = bubble_add, target = BubbleId}),
    log:bubble_log(RoleId, BubbleId, From, time:now()),
    {ok, FinalUser}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
