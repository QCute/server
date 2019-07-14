%%%-------------------------------------------------------------------
%%% @doc
%%% module battle skill script
%%% @end
%%%-------------------------------------------------------------------
-module(battle_effect).
%% API
-export([execute/6]).
%% Includes
-include("map.hrl").
-include("skill.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc execute effect
execute(Attacker, Target = #monster{attribute = Attribute = #attribute{hp = Hp}}, _Skill, _SkillData, Hurt, 1) ->
    NewHp = max(0, Hp - Hurt * 1.8),
    {Attacker, Target#monster{attribute = Attribute#attribute{hp = NewHp}}};
execute(Attacker, Target = #monster{attribute = Attribute = #attribute{hp = Hp}}, _Skill, _SkillData, Hurt, 2) ->
    NewHp = max(0, Hp - Hurt * 1.5),
    {Attacker, Target#monster{attribute = Attribute#attribute{hp = NewHp}}};
execute(Attacker, Target, _, _, _, _) ->
    {Attacker, Target}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
