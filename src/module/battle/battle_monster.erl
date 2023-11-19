%%%-------------------------------------------------------------------
%%% @doc
%%% battle monster
%%% @end
%%%-------------------------------------------------------------------
-module(battle_monster).
%% API
-export([attack/4]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("map.hrl").
-include("skill.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc attack start
-spec attack(State :: #map{}, Attacker :: #fighter{}, Skill :: #battle_skill{}, TargetList :: [#hatred{}]) -> {ok, #map{}} | error().
attack(State, Attacker, Skill, TargetList) ->
    Now = time:now(),
    %% check attacker can attack or not
    case battle_attribute:check(Attacker, cannot_attack) of
        false ->
            check_skill(State, Attacker, Skill, TargetList, Now);
        true ->
            {error, cannot_attack}
    end.

%% check attacker battle skill
check_skill(State, Attacker, Skill = #battle_skill{time = Time, cd = Cd}, TargetList, Now) ->
    case Time + Cd < Now of
        true ->
            perform_skill(State, Attacker, Skill, TargetList, Now);
        false ->
            {error, skill_cd}
    end.

%% perform skill
perform_skill(State, Attacker = #fighter{id = Id, skill = SkillList, x = X, y = Y}, Skill = #battle_skill{skill_id = SkillId}, TargetList, Now) ->
    case perform_skill_for_target_loop(State, Attacker, Skill, TargetList, Now, 0, []) of
        {NewState = #map{fighter = FighterList}, NewAttacker, _, List} ->
            %% update skill cd
            NewSkillList = lists:keyreplace(SkillId, #battle_skill.skill_id, SkillList, Skill#battle_skill{time = Now}),
            %% update attacker
            NewFighterList = lists:keyreplace(Id, #fighter.id, FighterList, NewAttacker#fighter{skill = NewSkillList}),
            %% notify target data to client
            {ok, AttackBinary} = user_router:encode(?PROTOCOL_MAP_ATTACK, [Id, SkillId, List]),
            map:notify(NewState, X, Y, AttackBinary),
            %% return new state
            {ok, NewState#map{fighter = NewFighterList}};
        Error ->
            Error
    end.

%% perform skill for each one target
perform_skill_for_target_loop(State, Attacker, _, [], _, Hurt, List) ->
    {State, Attacker, Hurt, List};
perform_skill_for_target_loop(State = #map{fighter = FighterList}, Attacker = #fighter{id = Id}, Skill = #battle_skill{distance = Distance}, [#hatred{id = TargetId} | TargetList], Now, Hurt, List) ->
    case check_target(State, Attacker, TargetId, Distance) of
        {ok, Target} ->
            %% base attribute hurt
            BaseHurt = battle_attribute:calculate_hurt(Attacker, Target),
            %% perform skill, calculate skill effect
            {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, BaseHurt),
            %% perform passive skill, calculate skill effect
            {FinalState, FinalAttacker, FinalTarget, FinalHurt} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
            case FinalTarget of
                #fighter{type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = 0}} ->
                    NewFighterList = lists:keydelete(TargetId, #fighter.id, FighterList);
                #fighter{type = ?MAP_OBJECT_MONSTER, data = FighterMonster = #fighter_monster{monster_type = MonsterType, hatreds = Hatreds}} ->
                    %% update target
                    NewHatreds = lists:sublist([#hatred{id = Id, type = ?MAP_OBJECT_MONSTER, subtype = MonsterType} | lists:keydelete(Id, #hatred.id, Hatreds)], 3),
                    NewFighterList = lists:keyreplace(TargetId, #fighter.id, FighterList, FinalTarget#fighter{data = FighterMonster#fighter_monster{hatreds = NewHatreds}});
                _ ->
                    %% update target
                    NewFighterList = lists:keyreplace(TargetId, #fighter.id, FighterList, FinalTarget)
            end,
            %% update hurt rank
            battle_rank:update(FinalState, FinalAttacker, FinalHurt, Now, hurt),
            %% handle battle event
            HandleEventState = handle_battle_event(FinalState#map{fighter = NewFighterList}, FinalAttacker, FinalTarget, FinalHurt),
            %% continue
            perform_skill_for_target_loop(HandleEventState, FinalAttacker, Skill, TargetList, Now, Hurt + FinalHurt, [FinalTarget | List]);
        _ ->
            perform_skill_for_target_loop(State, Attacker, Skill, TargetList, Now, Hurt, List)
    end.

%% find and check target attribute
check_target(State = #map{fighter = FighterList}, Attacker = #fighter{id = Id, camp = Camp}, TargetId, Distance) ->
    case lists:keyfind(TargetId, #fighter.id, FighterList) of
        false ->
            %% no such target
            {error, no_such_target};
        #fighter{id = Id} ->
            %% cannot attack self
            {error, cannot_attack_self};
        #fighter{attribute = #attribute{hp = 0}} ->
            %% filter dead object
            {error, dead_object};
        #fighter{camp = Camp} ->
            %% same camp
            {error, same_camp};
        Target = #fighter{} ->
            check_target_state(State, Attacker, Target, Distance)
    end.

check_target_state(State, Attacker, Target, Distance) ->
    %% check attacker can be-attack or not, and check they distance
    case battle_attribute:check(Target, cannot_be_attack) of
        false ->
            check_target_hit(State, Attacker, Target, Distance);
        true ->
            {error, cannot_be_attack}
    end.

check_target_hit(State, Attacker, Target, Distance) ->
    %% check attacker can be-attack or not, and check they distance
    case battle_attribute:calculate_hit(Attacker, Target) of
        true ->
            check_target_distance(State, Attacker, Target, Distance);
        false ->
            {error, attack_miss}
    end.

check_target_distance(_State, Attacker, Target, Distance) ->
    %% target is in skill distance
    case map:is_in_distance(Attacker, Target, Distance) of
        false ->
            {ok, Target};
        true ->
            {error, distance_far_away}
    end.

%% handle battle event
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = 0}}, Hurt) ->
    Events = [#battle_event{name = battle_monster_hurt, object = Attacker, target = Target, number = Hurt}, #battle_event{name = battle_monster_dead, object = Attacker, target = Target}],
    battle_event:trigger(State, Events);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_MONSTER}, Hurt) ->
    Event = #battle_event{name = battle_monster_hurt, object = Attacker, target = Target, number = Hurt},
    battle_event:trigger(State, Event);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_ROLE, attribute = #attribute{hp = 0}}, Hurt) ->
    Events = [#battle_event{name = battle_role_hurt, object = Attacker, target = Target, number = Hurt}, #battle_event{name = battle_role_dead, object = Attacker, target = Target}],
    battle_event:trigger(State, Events);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_ROLE}, Hurt) ->
    Event = #battle_event{name = battle_role_hurt, object = Attacker, target = Target, number = Hurt},
    battle_event:trigger(State, Event).
