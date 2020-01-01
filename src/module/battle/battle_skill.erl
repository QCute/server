%%%------------------------------------------------------------------
%%% @doc
%%% module battle skill
%%% @end
%%%------------------------------------------------------------------
-module(battle_skill).
%% API
-export([perform/5, perform_passive/5]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc perform skill
-spec perform(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{}, NewTarget :: #fighter{}}.
perform(State, Attacker, Target, Skill = #battle_skill{effect = Effect}, Hurt) ->
    execute_effect_loop(State, Attacker, Target, Skill, Hurt, Effect).

%% execute effect operation
execute_effect_loop(State, Attacker, Target, _Skill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
execute_effect_loop(State, Attacker, Target, Skill, Hurt, [Effect | T]) ->
    %% execute effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = battle_effect:execute(State, Attacker, Target, Skill, #battle_skill{}, Effect, Hurt),
    execute_effect_loop(NewState, NewAttacker, NewTarget, Skill, NewHurt, T).

%% @doc perform passive skill
-spec perform_passive(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{}, NewTarget :: #fighter{}}.
perform_passive(State, Attacker, Target = #fighter{skills = TargetSkillList}, Skill, Hurt) ->
    Now = time:ts(),
    perform_passive_loop(State, Attacker, Target, Skill, TargetSkillList, Hurt, Now, []).

perform_passive_loop(State, Attacker, Target = #fighter{attribute = Attribute = #attribute{hp = Hp}}, _, [], Hurt, _Now, NewSkillList) ->
    {State, Attacker, Target#fighter{skills = NewSkillList, attribute = Attribute#attribute{hp = max(0, Hp - Hurt)}}, Hurt};
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill = #battle_skill{type = passive, time = Time, effect = Effect} | T], Hurt, Now, NewSkillList) when Time < Now ->
    %% update skill cd
    NewPassiveSkill = PassiveSkill#battle_skill{time = Now},
    %% execute effect loop
    {NewState, NewAttacker, NewTarget, NewHurt} = execute_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, Effect),
    perform_passive_loop(NewState, NewAttacker, Skill, NewTarget, T, NewHurt, Now, [NewPassiveSkill | NewSkillList]);
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill | T], Hurt, Now, NewSkillList) ->
    perform_passive_loop(State, Attacker, Skill, Target, T, Hurt, Now, [PassiveSkill | NewSkillList]).

%% execute effect operation
execute_passive_effect_loop(State, Attacker, Target, _Skill, _PassiveSkill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
execute_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, [Effect | T]) ->
    %% execute effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = battle_effect:execute(State, Attacker, Target, Skill, PassiveSkill, Effect, Hurt),
    execute_passive_effect_loop(NewState, NewAttacker, NewTarget, Skill, PassiveSkill, NewHurt, T).

%%%==================================================================
%%% Internal functions
%%%==================================================================
