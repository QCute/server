%%%------------------------------------------------------------------
%%% @doc
%%% module buff
%%% @end
%%%------------------------------------------------------------------
-module(buff).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([add/2]).
-export([expire/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("buff.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Buff =  parser:convert(buff_sql:select(RoleId), ?MODULE),
    NewUser = lists:foldl(fun(#buff{buff_id = BuffId, overlap = Overlap}, Acc) -> user_effect:add(Acc, Overlap, (buff_data:get(BuffId))#buff_data.effect) end, User, Buff),
    NewUser#user{buff = Buff}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{buff = Buff}) ->
    NewSkill = buff_sql:insert_update(Buff),
    User#user{buff = NewSkill}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{buff = Buff}) ->
    {ok, Buff}.

%% @doc add
-spec add(User :: #user{}, BuffId :: non_neg_integer()) -> ok() | error().
add(User = #user{role_id = RoleId, buff = BuffList}, BuffId) ->
    Now = time:ts(),
    Buff = #buff{expire_time = EndTime, overlap = Overlap} = listing:key_find(BuffId, #buff.buff_id, BuffList, #buff{buff_id = BuffId, start_time = Now, overlap = 0}),
    case buff_data:get(BuffId) of
        #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_TIME, time = Time} ->
            NewBuff = Buff#buff{role_id = RoleId, expire_time = Time + EndTime, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            user_sender:send(User, ?PROTOCOL_BUFF, [Buff]),
            {ok, User#user{buff = NewBuffList}};
        #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_VALUE, effect = Effect} ->
            NewBuff = Buff#buff{role_id = RoleId, overlap = Overlap + 1, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            {ok, NewUser} = user_effect:add(User, NewBuff#buff.overlap, Effect),
            user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
            {ok, NewUser#user{buff = NewBuffList}};
        #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_ALL, time = Time, effect = Effect} ->
            NewBuff = Buff#buff{role_id = RoleId, expire_time = Time + EndTime, overlap = Overlap + 1, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            {ok, NewUser} = user_effect:add(User, NewBuff#buff.overlap, Effect),
            user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
            {ok, NewUser#user{buff = NewBuffList}};
        #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_NONE, time = Time, effect = Effect} when Buff#buff.role_id == 0 ->
            NewBuff = Buff#buff{role_id = RoleId, expire_time = Time + EndTime, overlap = 1, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            {ok, NewUser} = user_effect:add(User, NewBuff#buff.overlap, Effect),
            user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
            {ok, NewUser#user{buff = NewBuffList}};
        _ ->
            {error, configure_not_found}
    end.

%% @doc expire
-spec expire(#user{}) -> #user{}.
expire(User = #user{buff = BuffList}) ->
    Now = time:ts(),
    {NewUser, NewList, Delete} = expire_loop(BuffList, User, Now, [], []),
    user_sender:send(User, ?PROTOCOL_BUFF_DELETE, Delete),
    NewUser#user{buff = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #buff{role_id = RoleId, buff_id = BuffId, overlap = Overlap, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now =< ExpireTime of
        true ->
            buff_sql:delete(RoleId, BuffId),
            NewUser = user_effect:remove(User, Overlap, (buff_data:get(BuffId))#buff_data.effect),
            expire_loop(T, NewUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
