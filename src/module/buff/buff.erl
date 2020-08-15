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
-export([to_battle_buff/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("map.hrl").
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
            add_check_overlap(User, BuffData);
        _ ->
            {error, configure_not_found}
    end.

add_check_overlap(User = #user{buff = BuffList}, BuffData = #buff_data{buff_id = BuffId}) ->
    case lists:keyfind(BuffId, #buff.buff_id, BuffList) of
        Buff = #buff{} ->
            add_overlap(User, Buff, BuffData);
        _ ->
            add_new(User, BuffData)
    end.

%% add overlap to old buff
add_overlap(User, Buff = #buff{expire_time = ExpireTime}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_TIME, time = Time}) ->
    NewBuff = Buff#buff{expire_time = Time + ExpireTime, flag = 1},
    add_final(User, NewBuff, BuffData);
add_overlap(User, Buff = #buff{overlap = Overlap}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_VALUE, effect = Effect}) ->
    NewBuff = Buff#buff{overlap = Overlap + 1, flag = 1},
    NewUser = user_effect:add(User, 1, Effect),
    add_final(NewUser, NewBuff, BuffData);
add_overlap(User, Buff = #buff{expire_time = ExpireTime, overlap = Overlap}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_ALL, time = Time, effect = Effect}) ->
    NewBuff = Buff#buff{expire_time = Time + ExpireTime, overlap = Overlap + 1, flag = 1},
    NewUser = user_effect:add(User, 1, Effect),
    add_final(NewUser, NewBuff, BuffData);
add_overlap(_, _, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_NONE}) ->
    {error, duplicate_buff}.

%% new buff
add_new(User = #user{role_id = RoleId}, BuffData = #buff_data{buff_id = BuffId, time = 0, effect = Effect}) ->
    NewBuff = #buff{role_id = RoleId, buff_id = BuffId, flag = 1},
    NewUser = user_effect:add(User, 1, Effect),
    add_final(NewUser, NewBuff, BuffData);
add_new(User = #user{role_id = RoleId}, BuffData = #buff_data{buff_id = BuffId, time = Time, effect = Effect}) ->
    NewBuff = #buff{role_id = RoleId, buff_id = BuffId, expire_time = time:ts() + Time, flag = 1},
    NewUser = user_effect:add(User, 1, Effect),
    add_final(NewUser, NewBuff, BuffData).

%% save and push new/update buff info
add_final(User = #user{buff = BuffList}, Buff = #buff{buff_id = BuffId}, #buff_data{attribute = Attribute}) ->
    NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, Buff),
    NewUser = attribute:recalculate(User, {?MODULE, BuffId}, Attribute),
    user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
    {ok, NewUser#user{buff = NewBuffList}}.

%% @doc expire
-spec expire(#user{}) -> #user{}.
expire(User = #user{buff = BuffList}) ->
    Now = time:ts(),
    {NewUser, NewList, Delete} = expire_loop(BuffList, User, Now, [], []),
    FinalUser = attribute:calculate(NewUser),
    _ = Delete =/= [] andalso user_sender:send(FinalUser, ?PROTOCOL_BUFF_DELETE, Delete),
    FinalUser#user{buff = NewList}.

expire_loop([], User, _, List, Delete) ->
    {User, List, Delete};
expire_loop([Buff = #buff{expire_time = 0} | T], User, Now, List, Delete) ->
    expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #buff{role_id = RoleId, buff_id = BuffId, overlap = Overlap, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            buff_sql:delete(RoleId, BuffId),
            NewUser = attribute:remove(User, {?MODULE, BuffId}),
            NewestUser = user_effect:remove(NewUser, Overlap, (buff_data:get(BuffId))#buff_data.effect),
            expire_loop(T, NewestUser, Now, List, [Buff | Delete]);
        false ->
            expire_loop(T, User, Now, [Buff | List], Delete)
    end.

%% @doc convert buff id/buff to battle buff
-spec to_battle_buff([non_neg_integer() | #buff{}]) -> [#battle_buff{}].
to_battle_buff(List) ->
    to_battle_buff_loop(List, []).

to_battle_buff_loop([], List) ->
    List;
to_battle_buff_loop([#buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap} | T], List)  ->
    #buff_data{type = Type, effect = Effect} = buff_data:get(BuffId),
    to_battle_buff_loop(T, [#battle_buff{buff_id = BuffId, type = Type, expire_time = ExpireTime, overlap = Overlap, effect = Effect} | List]);
to_battle_buff_loop([BuffId | T], List)  ->
    #buff_data{type = Type, time = Time, effect = Effect} = buff_data:get(BuffId),
    to_battle_buff_loop(T, [#battle_buff{buff_id = BuffId, type = Type, expire_time = time:set_expire(Time), effect = Effect} | List]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
