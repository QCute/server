%%%-------------------------------------------------------------------
%%% @doc
%%% battle buff
%%% @end
%%%-------------------------------------------------------------------
-module(battle_buff).
%% API
-export([add/3]).
-export([loop/1]).
-export([calculate/5]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
-include("buff.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add
-spec add(State :: #map_state{}, Id :: non_neg_integer() | #fighter{}, BuffId :: non_neg_integer()) -> {ok, NewState :: #map_state{}} | {error, Reason :: term()}.
add(State = #map_state{fighter = FighterList}, Id, BuffId) ->
    case lists:keyfind(Id, #fighter.id, FighterList) of
        Fighter = #fighter{} ->
            add_check(State, Fighter, BuffId);
        _ ->
            {error, no_such_fighter}
    end.

add_check(State, Fighter, BuffId) ->
    case buff_data:get(BuffId) of
        BuffData = #buff_data{} ->
            add_check_overlap(State, Fighter, BuffData);
        _ ->
            {error, configure_not_found}
    end.

add_check_overlap(State, Fighter = #fighter{buff = BuffList}, BuffData = #buff_data{buff_id = BuffId}) ->
    case lists:keyfind(BuffId, #battle_buff.buff_id, BuffList) of
        Buff = #battle_buff{} ->
            add_overlap(State, Fighter, Buff, BuffData);
        _ ->
            add_new(State, Fighter, BuffData)
    end.

%% add overlap to old buff
add_overlap(State, Fighter, Buff = #battle_buff{expire_time = ExpireTime}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_TIME, expire_time = Time}) ->
    NewBuff = Buff#battle_buff{expire_time = Time + ExpireTime},
    add_final(State, Fighter, NewBuff, BuffData);
add_overlap(State, Fighter, Buff = #battle_buff{overlap = Overlap}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_VALUE}) ->
    NewBuff = Buff#battle_buff{overlap = Overlap + 1},
    %% calculate effect
    {NewState, NewFighter} = calculate_effect(State, Fighter, NewBuff),
    add_final(NewState, NewFighter, NewBuff, BuffData);
add_overlap(State, Fighter, Buff = #battle_buff{expire_time = ExpireTime, overlap = Overlap}, BuffData = #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_ALL, expire_time = Time}) ->
    NewBuff = Buff#battle_buff{expire_time = Time + ExpireTime, overlap = Overlap + 1},
    %% calculate effect
    {NewState, NewFighter} = calculate_effect(State, Fighter, NewBuff),
    add_final(NewState, NewFighter, NewBuff, BuffData);
add_overlap(_, _, _, #buff_data{overlap_type = ?BUFF_OVERLAP_TYPE_NONE}) ->
    {error, duplicate_buff}.

%% new buff
add_new(State, Fighter, BuffData = #buff_data{buff_id = BuffId, type = Type, expire_time = 0, effect = Effect}) ->
    %% no expire time
    NewBuff = #battle_buff{buff_id = BuffId, type = Type, effect = Effect},
    %% calculate effect
    {NewState, NewFighter} = calculate_effect(State, Fighter, NewBuff),
    add_final(NewState, NewFighter, NewBuff, BuffData);
add_new(State, Fighter, BuffData = #buff_data{buff_id = BuffId, type = Type, expire_time = Time, effect = Effect}) ->
    %% with expire time
    NewBuff = #battle_buff{buff_id = BuffId, type = Type, expire_time = time:now() + Time, effect = Effect},
    %% calculate effect
    {NewState, NewFighter} = calculate_effect(State, Fighter, NewBuff),
    add_final(NewState, NewFighter, NewBuff, BuffData).

%% save and push new/update buff info
add_final(State = #map_state{fighter = FighterList}, Fighter = #fighter{id = Id, buff = BuffList}, Buff = #battle_buff{buff_id = BuffId}, #buff_data{}) ->
    NewBuffList = lists:keystore(BuffId, #battle_buff.buff_id, BuffList, Buff),
    NewFighterList = lists:keystore(Id, #fighter.id, FighterList, Fighter#fighter{buff = NewBuffList}),
    {ok, State#map_state{fighter = NewFighterList}}.

%% calculate normal buff effect
calculate_effect(State, Fighter, #battle_buff{type = ?BUFF_TYPE_TIME}) ->
    {State, Fighter};
calculate_effect(State, Fighter, #battle_buff{effect = Effect}) ->
    %% calculate effect after add new buff or overlap
    effect_loop(Effect, add, 1, State, Fighter).

%% @doc buff loop
-spec loop(State :: #map_state{}) -> NewState :: #map_state{}.
loop(State = #map_state{fighter = FighterList}) ->
    fighter_loop(FighterList, State, time:now(), []).

%% calculate fighter buff
fighter_loop([], State, _, List) ->
    State#map_state{fighter = List};
fighter_loop([Fighter = #fighter{buff = BuffList} | T], State, Now, List) ->
    {NewState, NewFighter} = buff_loop(BuffList, State, Fighter, Now, []),
    fighter_loop(T, NewState, Now, [NewFighter | List]).

%% calculate buff effect
buff_loop([], State, Fighter, _, List) ->
    {State, Fighter#fighter{buff = List}};
%% time expire
buff_loop([#battle_buff{type = ?BUFF_TYPE_ATTRIBUTE, expire_time = ExpireTime, overlap = Overlap, effect = Effect} | T], State, Fighter, Now, List) when ExpireTime < Now ->
    {NewState, NewFighter} = effect_loop(Effect, reduce, Overlap, State, Fighter),
    buff_loop(T, NewState, NewFighter, Now, List);
%% time expire
buff_loop([#battle_buff{type = ?BUFF_TYPE_TIME, expire_time = ExpireTime} | T], State, Fighter, Now, List) when ExpireTime < Now ->
    buff_loop(T, State, Fighter, Now, List);
%% execute time buff effect
buff_loop([Buff = #battle_buff{type = ?BUFF_TYPE_TIME, overlap = Overlap, effect = Effect} | T], State, Fighter, Now, List) ->
    {NewState, NewFighter} = effect_loop(Effect, add, Overlap, State, Fighter),
    buff_loop(T, NewState, NewFighter, Now, [Buff | List]);
%% other buff
buff_loop([Buff | T], State, Fighter, Now, List) ->
    buff_loop(T, State, Fighter, Now, [Buff | List]).

%% calculate effect
effect_loop([], _, _, State, Fighter) ->
    {State, Fighter};
effect_loop([Effect | T], Operation, Overlap, State, Fighter) ->
    %% calculate effect script
    {NewState, NewFighter} = calculate(State, Fighter, Effect, Operation, Overlap),
    effect_loop(T, Operation, Overlap, NewState, NewFighter).

%% @doc calculate effect
-spec calculate(State :: #map_state{}, Fighter :: #fighter{}, EffectId :: non_neg_integer(), Operation :: add | reduce, Overlap :: non_neg_integer()) -> {NewState :: #map_state{}, NewFighter :: #fighter{}}.
calculate(State, Self, EffectId, reduce, Overlap) ->
    %% reduce do not check condition and ratio
    execute_script(EffectId, reduce, Overlap, State, Self);
calculate(State, Self, EffectId, Operation, Overlap) ->
    case check_condition(EffectId, State, Self) andalso randomness:hit(check_ratio(EffectId, State, Self)) of
        true ->
            execute_script(EffectId, Operation, Overlap, State, Self);
        false ->
            {State, Self}
    end.

%%%===================================================================
%%% buff trigger condition
%%%===================================================================
check_condition(5, _, _) ->
    true;

check_condition(_, _, _) ->
    false.

%%%===================================================================
%%% buff trigger ratio
%%%===================================================================
check_ratio(5, _State, _Self) ->
    10000;

check_ratio(_, _, _) ->
    0.

%%%===================================================================
%%% buff trigger effect
%%%===================================================================
%% effect script write @here

execute_script(4, add, _, State, Fighter = #fighter{attribute = Attribute}) ->
    {State, Fighter#fighter{attribute = Attribute#attribute{vertigo = 1}}};

execute_script(4, reduce, _, State, Fighter = #fighter{attribute = Attribute}) ->
    {State, Fighter#fighter{attribute = Attribute#attribute{vertigo = 0}}};

execute_script(5, _, Overlap, State, Fighter = #fighter{attribute = Attribute = #attribute{hp = Hp, health = Health}}) ->
    {State, Fighter#fighter{attribute = Attribute#attribute{hp = trunc(Hp - Health * Overlap * 0.01)}}};

execute_script(6, add, Overlap, State, Self = #fighter{attribute = Attribute = #attribute{attack = Attack}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = Attack * Overlap * 1.5}}};

execute_script(6, reduce, Overlap, State, Self = #fighter{attribute = Attribute = #attribute{attack = Attack}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = trunc(Attack / Overlap / 1.5)}}};

execute_script(7, add, Overlap, State, Self = #fighter{attribute = Attribute = #attribute{defense = Defense}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = Defense * Overlap * 2}}};

execute_script(7, reduce, Overlap, State, Self = #fighter{attribute = Attribute = #attribute{defense = Defense}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = trunc(Defense / Overlap / 2)}}};

execute_script(_, _, _, State, Fighter) ->
    {State, Fighter}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
