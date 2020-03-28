%%%------------------------------------------------------------------
%%% @doc
%%% module battle fighter
%%% @end
%%%------------------------------------------------------------------
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
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc attack start
-spec attack(State :: #map_state{}, Attacker :: #fighter{}, Skill :: #battle_skill{}, TargetList :: [#hatred{}]) -> {ok, #map_state{}} | error().
attack(State, Attacker, Skill, TargetList) ->
    Now = time:ts(),
    case validate_attacker(State, Attacker, Skill, Now) of
        ok ->
            perform_skill(State, Attacker, Skill, TargetList, Now);
        Error ->
            Error
    end.

%% validate attacker
validate_attacker(State, Attacker = #fighter{}, SkillId, Now) ->
    %% check attacker can attack or not
    case battle_attribute:check(Attacker, cannot_attack) of
        false ->
            validate_skill(State, Attacker, SkillId, Now);
        true ->
            {error, cannot_attack}
    end.

%% validate attacker battle skill
validate_skill(_State, _Attacker, #battle_skill{time = Time, cd = Cd}, Now) ->
    case Time + Cd < Now of
        true ->
            ok;
        false ->
            {error, skill_cd}
    end.

%% perform skill
perform_skill(State, Attacker = #fighter{id = Id, skills = Skills, x = X, y = Y}, Skill = #battle_skill{skill_id = SkillId}, TargetList, Now) ->
    case perform_skill_loop(State, Attacker, Skill, TargetList, Now, 0, []) of
        {NewState = #map_state{fighters = Fighters}, NewAttacker, _, List} ->
            %% update skill cd
            NewSkills = lists:keyreplace(SkillId, #battle_skill.skill_id, Skills, Skill#battle_skill{time = Now}),
            %% update attacker
            NewFighters = lists:keyreplace(Id, #fighter.id, Fighters, NewAttacker#fighter{skills = NewSkills}),
            %% notify target data to client
            {ok, Binary} = user_router:write(?PROTOCOL_MAP_FIGHTER, List),
            map:notify(NewState, X, Y, Binary),
            %% return new state
            {ok, NewState#map_state{fighters = NewFighters}};
        Error ->
            Error
    end.

%% perform skill for each one target
perform_skill_loop(State, Attacker, _, [], _, Hurt, List) ->
    {State, Attacker, Hurt, List};
perform_skill_loop(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id, monster_type = MonsterType}, Skill = #battle_skill{distance = Distance}, [#hatred{id = TargetId} | TargetList], Now, Hurt, List) ->
    case validate_target(State, Attacker, TargetId, Distance) of
        {ok, Target= #fighter{hatreds = Hatreds}} ->
            %% base attribute hurt
            BaseHurt = battle_attribute:calculate_hurt(Attacker, Target),
            %% perform skill, calculate skill effect
            {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, BaseHurt),
            %% perform passive skill, calculate skill effect
            {FinalState, FinalAttacker, FinalTarget, FinalHurt} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
            case FinalTarget of
                #fighter{type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = 0}} ->
                    NewFighters = lists:keydelete(TargetId, #fighter.id, Fighters);
                _ ->
                    %% update target
                    NewHatreds = lists:sublist(lists:keystore(Id, #hatred.id, Hatreds, #hatred{id = Id, type = ?MAP_OBJECT_MONSTER, subtype = MonsterType}), 3),
                    NewFighters = lists:keyreplace(TargetId, #fighter.id, Fighters, FinalTarget#fighter{hatreds = NewHatreds})
            end,
            %% update hurt rank
            battle_rank:update(NewState, NewAttacker, FinalHurt, Now, hurt),
            %% handle battle event
            HandleEventState = handle_battle_event(FinalState#map_state{fighters = NewFighters}, FinalAttacker, FinalTarget, FinalHurt),
            %% continue
            perform_skill_loop(HandleEventState, FinalAttacker, Skill, TargetList, Now, Hurt + FinalHurt, [FinalTarget | List]);
        Error ->
            Error
    end.

%% find and check target attribute
validate_target(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id, camp = Camp}, TargetId, Distance) ->
    case lists:keyfind(TargetId, #fighter.id, Fighters) of
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
            {error, same_cap};
        Target = #fighter{} ->
            validate_target_state(State, Attacker, Target, Distance)
    end.

validate_target_state(State, Attacker, Target, Distance) ->
    %% check attacker can be-attack or not, and check they distance
    case not battle_attribute:check(Target, cannot_be_attack) of
        true ->
            validate_target_distance(State, Attacker, Target, Distance);
        false ->
            {error, cannot_be_attack}
    end.

validate_target_distance(_State, Attacker, Target, Distance) ->
    %% target is in skill distance
    case map:is_in_distance(Attacker, Target, Distance) of
        true ->
            {ok, Target};
        false ->
            {error, distance_far_away}
    end.

%% handle battle event
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = 0}}, Hurt) ->
    Events = [#battle_event{name = event_battle_monster_hurt, object = Attacker, target = Target, number = Hurt}, #battle_event{name = event_battle_monster_dead, object = Attacker, target = Target}],
    battle_event:handle(State, Events);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_MONSTER}, Hurt) ->
    Event = #battle_event{name = event_battle_monster_hurt, object = Attacker, target = Target, number = Hurt},
    battle_event:handle(State, Event);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_ROLE, attribute = #attribute{hp = 0}}, Hurt) ->
    Events = [#battle_event{name = event_battle_role_hurt, object = Attacker, target = Target, number = Hurt}, #battle_event{name = event_battle_role_dead, object = Attacker, target = Target}],
    battle_event:handle(State, Events);
handle_battle_event(State, Attacker, Target = #fighter{type = ?MAP_OBJECT_ROLE}, Hurt) ->
    Event = #battle_event{name = event_battle_role_hurt, object = Attacker, target = Target, number = Hurt},
    battle_event:handle(State, Event).
