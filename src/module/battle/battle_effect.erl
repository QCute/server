%%%-------------------------------------------------------------------
%%% @doc
%%% module battle skill script
%%% @end
%%%-------------------------------------------------------------------
-module(battle_effect).
%% API
-export([execute/7]).
%% Includes
-include("map.hrl").
-include("skill.hrl").
-include("buff.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc execute effect
execute(State, Self, Rival, _Skill, _PassiveSkill, Hurt, EffectId) ->
    case check_condition(EffectId, State, Self, Rival, Hurt) andalso check_ratio(EffectId, State, Self, Rival, Hurt) of
        true ->
            execute_script(EffectId, State, Self, Rival, Hurt);
        false ->
            {State, Self, Rival, Hurt}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_condition(3, _State, Self, _Rival, _Hurt) ->
    Self#fighter.attribute#attribute.hp == 0;

check_condition(4, _State, _Self, Rival, _Hurt) ->
    Rival#fighter.attribute#attribute.hp == 0;

check_condition(_, _, _, _, _) ->
    false.


check_ratio(1, _State, _Self, _Rival, _Hurt) ->
    1000;

check_ratio(_, _, _, _, _) ->
    0.


execute_script(1, State, Self, Rival = #fighter{attribute = Attribute = #attribute{hp = Hp}}, Hurt) ->
    {State, Self, Rival#fighter{attribute = Attribute#attribute{hp = max(0, Hp - (Hurt * 1.8))}}, Hurt * 1.8};

execute_script(2, State, Self, Rival = #fighter{attribute = Attribute = #attribute{hp = Hp}}, Hurt) ->
    {State, Self, Rival#fighter{attribute = Attribute#attribute{hp = max(0, Hp - (Hurt * 1.5))}}, Hurt * 1.5};

execute_script(3, State, Self = #fighter{attribute = Attribute}, Rival, Hurt) ->
    {State, Self#fighter{attribute = Attribute#attribute{hp = Self#fighter.attribute#attribute.health}}, Rival, Hurt};

execute_script(4, State, Self = #fighter{attribute = Attribute}, Rival, Hurt) ->
    {State, Self#fighter{attribute = Attribute#attribute{vertigo = 0}}, Rival, Hurt};

execute_script(5, State, Self = #fighter{attribute = Attribute = #attribute{attack = Attack}}, Rival, Hurt) ->
    {State, Self#fighter{attribute = Attribute#attribute{attack = Attack * 1.5}}, Rival, Hurt};

execute_script(6, State, Self = #fighter{attribute = Attribute = #attribute{defense = Defense}}, Rival, Hurt) ->
    {State, Self#fighter{attribute = Attribute#attribute{defense = Defense * 2}}, Rival, Hurt};

execute_script(_, State, Self, Rival, Hurt) ->
    {State, Self, Rival, Hurt}.


