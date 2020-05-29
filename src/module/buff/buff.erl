%%%-------------------------------------------------------------------
%%% @doc
%%% module buff
%%% @end
%%%-------------------------------------------------------------------
-module(buff).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([add/2]).
-export([expire/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("buff.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Buff = buff_sql:select(RoleId),
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
add(User, BuffId) ->
    case buff_data:get(BuffId) of
        BuffData = #buff_data{} ->
            add_check(User, BuffData);
        _ ->
            {error, configure_not_found}
    end.

add_check(User = #user{buff = BuffList}, BuffData = #buff_data{buff_id = BuffId}) ->
    case lists:keyfind(BuffId, #buff.buff_id, BuffList) of
        Buff = #buff{} ->
            add_overlap(User, Buff, BuffData);
        _ ->
            add_new(User, BuffData)
    end.

%% add overlap to old buff
add_overlap(User, Buff = #buff{expire_time = ExpireTime}, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_TIME, time = Time}) ->
    NewBuff = Buff#buff{expire_time = Time + ExpireTime, flag = 1},
    add_final(User, NewBuff);
add_overlap(User, Buff = #buff{overlap = Overlap}, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_VALUE, effect = Effect}) ->
    NewBuff = Buff#buff{overlap = Overlap + 1, flag = 1},
    NewUser = user_effect:add(User, NewBuff#buff.overlap, Effect),
    add_final(NewUser, NewBuff);
add_overlap(User, Buff = #buff{expire_time = ExpireTime, overlap = Overlap}, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_ALL, time = Time, effect = Effect}) ->
    NewBuff = Buff#buff{expire_time = Time + ExpireTime, overlap = Overlap + 1, flag = 1},
    NewUser = user_effect:add(User, NewBuff#buff.overlap, Effect),
    add_final(NewUser, NewBuff);
add_overlap(_, _, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_NONE}) ->
    {error, duplicate_buff}.

%% new buff
add_new(User = #user{role_id = RoleId}, #buff_data{buff_id = BuffId, time = 0, effect = Effect}) ->
    NewBuff = #buff{role_id = RoleId, buff_id = BuffId, overlap = 1, flag = 1},
    NewUser = user_effect:add(User, NewBuff#buff.overlap, Effect),
    add_final(NewUser, NewBuff);
add_new(User = #user{role_id = RoleId}, #buff_data{buff_id = BuffId, time = Time, effect = Effect}) ->
    NewBuff = #buff{role_id = RoleId, buff_id = BuffId, overlap = 1, expire_time = time:ts() + Time, flag = 1},
    NewUser = user_effect:add(User, NewBuff#buff.overlap, Effect),
    add_final(NewUser, NewBuff).

%% save and push new/update buff info
add_final(User = #user{buff = BuffList}, Buff = #buff{overlap = Overlap}) ->
    NewBuffList = lists:keystore(Overlap, #buff.buff_id, BuffList, Buff),
    user_sender:send(User, ?PROTOCOL_BUFF, [Buff]),
    {ok, User#user{buff = NewBuffList}}.

%% @doc expire
-spec expire(#user{}) -> #user{}.
expire(User = #user{buff = BuffList}) ->
    Now = time:ts(),
    {NewUser, NewList, Delete} = expire_loop(BuffList, User, Now, [], []),
    _ = Delete =/= [] andalso user_sender:send(User, ?PROTOCOL_BUFF_DELETE, Delete),
    NewUser#user{buff = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #buff{expire_time = 0} | T], User, Now, List, Delete) ->
    expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #buff{role_id = RoleId, buff_id = BuffId, overlap = Overlap, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            buff_sql:delete(RoleId, BuffId),
            NewUser = user_effect:remove(User, Overlap, (buff_data:get(BuffId))#buff_data.effect),
            expire_loop(T, NewUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
