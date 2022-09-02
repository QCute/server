%%%-------------------------------------------------------------------
%%% @doc
%%% battle skill
%%% @end
%%%-------------------------------------------------------------------
-module(battle_skill).
%% API
-export([launch/3]).
-export([perform/5, perform_passive/5]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("skill.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc launch skill
-spec launch(State :: #map_state{}, Fighter :: #fighter{}, SkillId :: non_neg_integer()) -> {NewState :: #map_state{}, NewFighter :: #fighter{}}.
launch(State = #map_state{fighter = FighterList}, Fighter = #fighter{id = Id, skill = SkillList}, SkillId) ->
    Now = time:now(),
    case lists:keyfind(SkillId, #battle_skill.skill_id, SkillList) of
        Skill = #battle_skill{time = Time, cd = Cd, effect = Effect} when Time + Cd =< Now ->
            %% effect
            {NewState, NewFighter, _, _} = calculate_effect_loop(State, Fighter, Fighter, Skill, 0, Effect),
            %% update cd
            NewSkillList = lists:keyreplace(SkillId, #battle_skill.skill_id, SkillList, Skill#battle_skill{time = Now}),
            %% update fighter
            NewFighterList = lists:keystore(Id, #fighter.id, FighterList, NewFighter#fighter{skill = NewSkillList}),
            {ok, NewState#map_state{fighter = NewFighterList}};
        #battle_skill{} ->
            {error, skill_cd};
        false ->
            {error, no_such_skill}
    end.

%% @doc perform skill
-spec perform(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewState :: #map_state{}, NewAttacker :: #fighter{}, NewTarget :: #fighter{}, NewHurt :: non_neg_integer()}.
perform(State, Attacker, Target, Skill = #battle_skill{effect = Effect}, Hurt) ->
    calculate_effect_loop(State, Attacker, Target, Skill, Hurt, Effect).

%% calculate effect operation
calculate_effect_loop(State, Attacker, Target, _Skill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
calculate_effect_loop(State, Attacker, Target, Skill, Hurt, [Effect | T]) ->
    %% calculate effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = calculate(State, Attacker, Target, Skill, #battle_skill{}, Hurt, Effect),
    calculate_effect_loop(NewState, NewAttacker, NewTarget, Skill, NewHurt, T).

%% @doc perform passive skill
-spec perform_passive(State :: #map_state{}, Attacker :: #fighter{}, Target :: #fighter{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewState :: #map_state{}, NewAttacker :: #fighter{}, NewTarget :: #fighter{}, NewHurt :: non_neg_integer()}.
perform_passive(State, Attacker, Target = #fighter{skill = TargetSkillList}, Skill, Hurt) ->
    Now = time:now(),
    perform_passive_loop(State, Attacker, Target, Skill, TargetSkillList, Hurt, Now, []).

perform_passive_loop(State, Attacker, Target, _, [], Hurt, _, NewSkillList) ->
    {State, Attacker, Target#fighter{skill = NewSkillList}, Hurt};
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill = #battle_skill{type = passive, time = Time, cd = Cd, effect = Effect} | T], Hurt, Now, NewSkillList) when Time < Now ->
    %% calculate effect loop
    {NewState, NewAttacker, NewTarget, NewHurt} = calculate_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, Effect),
    %% update skill cold time
    NewPassiveSkill = PassiveSkill#battle_skill{time = Now + Cd},
    perform_passive_loop(NewState, NewAttacker, Skill, NewTarget, T, NewHurt, Now, [NewPassiveSkill | NewSkillList]);
perform_passive_loop(State, Attacker, Target, Skill, [PassiveSkill | T], Hurt, Now, NewSkillList) ->
    perform_passive_loop(State, Attacker, Target, Skill, T, Hurt, Now, [PassiveSkill | NewSkillList]).

%% calculate effect operation
calculate_passive_effect_loop(State, Attacker, Target, _Skill, _PassiveSkill, Hurt, []) ->
    {State, Attacker, Target, Hurt};
calculate_passive_effect_loop(State, Attacker, Target, Skill, PassiveSkill, Hurt, [Effect | T]) ->
    %% calculate effect script
    {NewState, NewAttacker, NewTarget, NewHurt} = calculate(State, Attacker, Target, Skill, PassiveSkill, Hurt, Effect),
    calculate_passive_effect_loop(NewState, NewAttacker, NewTarget, Skill, PassiveSkill, NewHurt, T).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% calculate effect
calculate(State, Self, Rival, _Skill, _PassiveSkill, Hurt, EffectId) ->
    case check_condition(EffectId, State, Self, Rival, Hurt) andalso randomness:hit(check_ratio(EffectId, State, Self, Rival, Hurt)) of
        true ->
            execute_script(EffectId, State, Self, Rival, Hurt);
        false ->
            {State, Self, Rival, Hurt}
    end.

%%%===================================================================
%%% skill trigger condition
%%%===================================================================
%% normal skill
check_condition(1, _State, _Self, _Rival, _Hurt) ->
    true;

check_condition(3, _State, Self, _Rival, _Hurt) ->
    Self#fighter.attribute#attribute.hp == 0;

check_condition(4, _State, _Self, Rival, _Hurt) ->
    Rival#fighter.attribute#attribute.hp == 0;

check_condition(8, _State, _Self, _Rival, _Hurt) ->
    true;

check_condition(_, _, _, _, _) ->
    false.

%%%===================================================================
%%% skill trigger ratio
%%%===================================================================
%% normal skill
check_ratio(1, _State, _Self, _Rival, _Hurt) ->
    10000;

check_ratio(8, _State, _Self, _Rival, _Hurt) ->
    10000;

check_ratio(_, _, _, _, _) ->
    0.

%%%===================================================================
%%% skill trigger effect
%%%===================================================================
%% normal skill
execute_script(1, State, Self, Rival = #fighter{attribute = Attribute = #attribute{hp = Hp}}, Hurt) ->
    {State, Self, Rival#fighter{attribute = Attribute#attribute{hp = max(0, Hp - Hurt)}}, Hurt};

execute_script(2, State, Self, Rival = #fighter{attribute = Attribute = #attribute{hp = Hp}}, Hurt) ->
    {State, Self, Rival#fighter{attribute = Attribute#attribute{hp = max(0, Hp - (Hurt * 1.5))}}, Hurt * 1.5};

execute_script(3, State, Self = #fighter{attribute = Attribute}, Rival, Hurt) ->
    {State, Self#fighter{attribute = Attribute#attribute{hp = Self#fighter.attribute#attribute.health}}, Rival, Hurt};

execute_script(8, State, Self, Rival = #fighter{id = Id}, Hurt) ->
    case battle_buff:add(State, Rival, 6) of
        {ok, NewState = #map_state{fighter = FighterList}} ->
            NewRival = lists:keyfind(Id, #fighter.id, FighterList),
            {NewState, Self, NewRival, Hurt};
        _ ->
            {State, Self, Rival, Hurt}
    end;

execute_script(_, State, Self, Rival, Hurt) ->
    {State, Self, Rival, Hurt}.

