%%%-------------------------------------------------------------------
%%% @doc
%%% module battle buff
%%% @end
%%%-------------------------------------------------------------------
-module(battle_buff).
%% API
-export([add/3]).
-export([loop/1]).
-export([calculate/4]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
-include("buff.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add
-spec add(State :: #map_state{}, Id :: non_neg_integer() | #fighter{}, BuffId :: non_neg_integer()) -> {NewState :: #map_state{}, NewFighter :: #fighter{}} | {error, Reason :: term()}.
add(State, Fighter = #fighter{buffs = Buffs}, BuffId) ->
    [Buff] = buff:to_battle_buff([BuffId]),
    calculate_effect(State, Fighter#fighter{buffs = [Buff | Buffs]}, Buff);
add(State = #map_state{fighters = Fighters}, Id, BuffId) ->
    case lists:keyfind(Id, #fighter.id, Fighters) of
        Fighter = #fighter{buffs = Buffs} ->
            [Buff] = buff:to_battle_buff([BuffId]),
            calculate_effect(State, Fighter#fighter{buffs = [Buff | Buffs]}, Buff);
        _ ->
            {error, no_such_fighter}
    end.

%% calculate normal buff effect
calculate_effect(State, Fighter, #battle_buff{type = ?BUFF_TYPE_TIME}) ->
    {State, Fighter};
calculate_effect(State, Fighter, #battle_buff{effect = Effect}) ->
    effect_loop(Effect, add, State, Fighter).

%% @doc buff loop
-spec loop(State :: #map_state{}) -> NewState :: #map_state{}.
loop(State = #map_state{fighters = Fighters}) ->
    fighter_loop(Fighters, State, time:ts(), []).

%% calculate fighter buffs
fighter_loop([], State, _, List) ->
    State#map_state{fighters = List};
fighter_loop([Fighter = #fighter{buffs = Buffs} | T], State, Now, List) ->
    {NewState, NewFighter} = buff_loop(Buffs, State, Fighter, Now, []),
    fighter_loop(T, NewState, Now, [NewFighter | List]).

%% calculate buff effect
buff_loop([], State, Fighter, _, List) ->
    {State, Fighter#fighter{buffs = List}};
%% time expire
buff_loop([#battle_buff{expire_time = ExpireTime, effect = Effect} | T], State, Fighter, Now, List) when ExpireTime < Now ->
    {NewState, NewFighter} = effect_loop(Effect, reduce, State, Fighter),
    buff_loop(T, NewState, NewFighter, Now, List);
%% execute time buff effect
buff_loop([Buff = #battle_buff{type = ?BUFF_TYPE_TIME, effect = Effect} | T], State, Fighter, Now, List) ->
    {NewState, NewFighter} = effect_loop(Effect, add, State, Fighter),
    buff_loop(T, NewState, NewFighter, Now, [Buff | List]);
%% other buff
buff_loop([Buff | T], State, Fighter, Now, List) ->
    buff_loop(T, State, Fighter, Now, [Buff | List]).

%% calculate effect
effect_loop([], _, State, Fighter) ->
    {State, Fighter};
effect_loop([Effect | T], Operation, State, Fighter) ->
    %% calculate effect script
    {NewState, NewFighter} = calculate(State, Fighter, Effect, Operation),
    effect_loop(T, Operation, NewState, NewFighter).

%% @doc calculate effect
-spec calculate(State :: #map_state{}, Fighter :: #fighter{}, EffectId :: non_neg_integer(), Operation :: add | reduce) -> {NewState :: #map_state{}, NewFighter :: #fighter{}}.
calculate(State, Self, EffectId, reduce) ->
    %% reduce do'not check condition and ratio
    execute_script(EffectId, reduce, State, Self);
calculate(State, Self, EffectId, Operation) ->
    case check_condition(EffectId, State, Self) andalso randomness:hit(check_ratio(EffectId, State, Self)) of
        true ->
            execute_script(EffectId, Operation, State, Self);
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
%% effect script write here

execute_script(5, _, State, Fighter = #fighter{attribute = Attribute = #attribute{hp = Hp, health = Health}}) ->
    {State, Fighter#fighter{attribute = Attribute#attribute{hp = trunc(Hp - Health * 0.01)}}};

execute_script(6, add, State, Self = #fighter{attribute = Attribute = #attribute{attack = Attack}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = Attack * 1.5}}};

execute_script(6, reduce, State, Self = #fighter{attribute = Attribute = #attribute{attack = Attack}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = trunc(Attack / 1.5)}}};

execute_script(7, add, State, Self = #fighter{attribute = Attribute = #attribute{defense = Defense}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = Defense * 2}}};

execute_script(7, reduce, State, Self = #fighter{attribute = Attribute = #attribute{defense = Defense}}) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = trunc(Defense / 2)}}};

execute_script(_, _, State, Fighter) ->
    {State, Fighter}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
