%%%------------------------------------------------------------------
%%% @doc
%%% module battle skill
%%% @end
%%%------------------------------------------------------------------
-module(battle_skill).
%% API
-export([perform/5, perform_passive/5]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("attribute.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc perform skill
-spec perform(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{}, NewTarget :: #fighter{}}.
perform(State, Attacker, Target, Skill = #battle_skill{effect = Effect}, Hurt) ->
    calculate_effect_loop(State, Attacker, Target, Skill, Hurt, Effect).

%% calculate effect operation
calculate_effect_loop(State, Attacker, Target, _Skill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
calculate_effect_loop(State, Attacker, Target, Skill, Hurt, [Effect | T]) ->
    %% calculate effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = battle_effect:calculate(State, Attacker, Target, Skill, #battle_skill{}, Hurt, Effect),
    calculate_effect_loop(NewState, NewAttacker, NewTarget, Skill, NewHurt, T).

%% @doc perform passive skill
-spec perform_passive(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{}, NewTarget :: #fighter{}}.
perform_passive(State, Attacker, Target = #fighter{skills = TargetSkillList}, Skill, Hurt) ->
    Now = time:ts(),
    perform_passive_loop(State, Attacker, Target, Skill, TargetSkillList, Hurt, Now, []).

perform_passive_loop(State, Attacker, Target = #fighter{attribute = Attribute = #attribute{hp = Hp}}, _, [], Hurt, _, NewSkillList) ->
    {State, Attacker, Target#fighter{skills = NewSkillList, attribute = Attribute#attribute{hp = max(0, Hp - Hurt)}}, Hurt};
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill = #battle_skill{type = passive, time = Time, effect = Effect} | T], Hurt, Now, NewSkillList) when Time < Now ->
    %% update skill cd
    NewPassiveSkill = PassiveSkill#battle_skill{time = Now},
    %% calculate effect loop
    {NewState, NewAttacker, NewTarget, NewHurt} = calculate_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, Effect),
    perform_passive_loop(NewState, NewAttacker, Skill, NewTarget, T, NewHurt, Now, [NewPassiveSkill | NewSkillList]);
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill | T], Hurt, Now, NewSkillList) ->
    perform_passive_loop(State, Attacker, Target, Skill, T, Hurt, Now, [PassiveSkill | NewSkillList]).

%% calculate effect operation
calculate_passive_effect_loop(State, Attacker, Target, _Skill, _PassiveSkill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
calculate_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, [Effect | T]) ->
    %% calculate effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = battle_effect:calculate(State, Attacker, Target, Skill, PassiveSkill, Hurt, Effect),
    calculate_passive_effect_loop(NewState, NewAttacker, NewTarget, Skill, PassiveSkill, NewHurt, T).

%%%==================================================================
%%% Internal functions
%%%==================================================================
