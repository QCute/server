%%%-------------------------------------------------------------------
%%% @doc
%%% module battle skill
%%% @end
%%%-------------------------------------------------------------------
-module(battle_skill).
%% API
-export([perform/4, perform_passive/4]).
%% Includes
-include("map.hrl").
-include("skill.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc perform skill
-spec perform(Attacker :: #fighter{} | #monster{}, Target :: #fighter{} | #monster{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{} | #monster{}, NewTarget :: #fighter{} | #monster{}}.
perform(Attacker, Target, Skill = #battle_skill{skill_id = SkillId}, Hurt) ->
    SkillData = skill_data:get(SkillId),
    perform_loop(Attacker, Target, Skill, SkillData, Hurt, []).

%% execute effect operation
perform_loop(Attacker, Target, _Skill, _SkillData, _Hurt, []) ->
    {Attacker, Target};
perform_loop(Attacker, Target, Skill, SkillData, Hurt, [Effect | T]) ->
    %% execute effect script
    {NewAttacker, NewTarget} = battle_effect:execute(Attacker, Target, Skill, SkillData, Effect, Hurt),
    perform_loop(NewAttacker, NewTarget, Skill, SkillData, Hurt, T).

%% @doc perform passive skill
-spec perform_passive(Attacker :: #fighter{} | #monster{}, Target :: #fighter{} | #monster{}, Skill :: #battle_skill{}, Hurt :: non_neg_integer()) -> {NewAttacker :: #fighter{} | #monster{}, NewTarget :: #fighter{} | #monster{}}.
perform_passive(Attacker, Target, Skill = #battle_skill{skill_id = SkillId}, Hurt) ->
    SkillData = skill_data:get(SkillId),
    perform_passive_loop(Attacker, Target, Skill, SkillData, Hurt, []).

%% execute effect operation
perform_passive_loop(Attacker, Target, _Skill, _SkillData, _Hurt, []) ->
    {Attacker, Target};
perform_passive_loop(Attacker, Target, Skill, SkillData, Hurt, [Effect | T]) ->
    %% execute effect script
    {NewAttacker, NewTarget} = battle_effect:execute(Attacker, Target, Skill, SkillData, Effect, Hurt),
    perform_passive_loop(NewAttacker, NewTarget, Skill, SkillData, Hurt, T).

%%%===================================================================
%%% Internal functions
%%%===================================================================
