%%%-------------------------------------------------------------------
%%% @doc
%%% module battle role
%%% @end
%%%-------------------------------------------------------------------
-module(battle_role).
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
-spec attack(State :: #map_state{}, AttackerId :: non_neg_integer(), SkillId :: non_neg_integer(), TargetList :: [non_neg_integer()]) -> {ok, #map_state{}} | error().
attack(State, AttackerId, SkillId, TargetList) ->
    Now = time:ts(),
    case check_attacker(State, AttackerId, SkillId, TargetList, Now) of
        {ok, Attacker, Skill} ->
            perform_skill(State, Attacker, Skill, TargetList, Now);
        Error ->
            Error
    end.

%% check attacker
check_attacker(State = #map_state{fighter = FighterList}, AttackerId, SkillId, TargetList, Now) ->
    case lists:keyfind(AttackerId, #fighter.id, FighterList) of
        Attacker = #fighter{attribute = #attribute{}} ->
            check_attacker_state(State, Attacker, SkillId, TargetList, Now);
        false ->
            {error, no_such_attacker}
    end.

check_attacker_state(State, Attacker, SkillId, TargetList, Now) ->
    %% check attacker can attack or not
    case battle_attribute:check(Attacker, cannot_attack) of
        false ->
            check_skill(State, Attacker, SkillId, TargetList, Now);
        true ->
            {error, cannot_attack}
    end.

%% check attacker battle skill
check_skill(State, Attacker = #fighter{skill = SkillList}, SkillId, TargetList, Now) ->
    case lists:keyfind(SkillId, #battle_skill.skill_id, SkillList) of
        Skill = #battle_skill{cd = Cd} when Cd =< Now ->
            check_target_number(State, Attacker, Skill, TargetList);
        #battle_skill{} ->
            {error, skill_cd};
        false ->
            {error, no_such_skill}
    end.

%% check skill effect number great then or equals target number
check_target_number(_State, Attacker, Skill = #battle_skill{number = Number}, TargetList) ->
    case length(TargetList) =< Number of
        true ->
            {ok, Attacker, Skill};
        false ->
            {error, too_many_target}
    end.

%% perform skill
perform_skill(State, Attacker = #fighter{id = Id, sender_pid = SenderPid, skill = SkillList, x = X, y = Y}, Skill = #battle_skill{skill_id = SkillId, cd = SkillCd}, TargetList, Now) ->
    {NewState = #map_state{fighter = FighterList}, NewAttacker, _, List} = perform_skill_loop(State, Attacker, Skill, TargetList, Now, 0, []),
    %% update skill cd
    NewSkillList = lists:keyreplace(SkillId, #battle_skill.skill_id, SkillList, Skill#battle_skill{time = Now + SkillCd}),
    FinalAttacker = NewAttacker#fighter{skill = NewSkillList},
    %% update self data
    user_sender:send(SenderPid, ?PROTOCOL_MAP_SELF, FinalAttacker),
    %% update attacker
    NewFighterList = lists:keyreplace(Id, #fighter.id, FighterList, FinalAttacker),
    %% notify target data to client
    {ok, Binary} = user_router:write(?PROTOCOL_MAP_FIGHTER, List),
    map:notify(NewState, X, Y, Binary),
    %% return new state
    {ok, NewState#map_state{fighter = NewFighterList}}.

%% perform skill for each one target
perform_skill_loop(State, Attacker, _, [], _, Hurt, List) ->
    {State, Attacker, Hurt, List};
perform_skill_loop(State = #map_state{fighter = FighterList}, Attacker = #fighter{id = Id}, Skill = #battle_skill{distance = Distance}, [TargetId | TargetList], Now, Hurt, List) ->
    case check_target(State, Attacker, TargetId, Distance) of
        {ok, Target = #fighter{hatreds = Hatreds}} ->
            %% base attribute hurt
            BaseHurt = battle_attribute:calculate_hurt(Attacker, Target),
            %% perform skill, calculate skill effect
            {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, BaseHurt),
            %% perform passive skill, calculate skill effect
            {FinalState, FinalAttacker, FinalTarget, FinalHurt} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
            %% update or delete
            case FinalTarget of
                #fighter{type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = 0}} ->
                    %% if the target is monster and it is dead, remove it
                    NewFighterList = lists:keydelete(TargetId, #fighter.id, FighterList);
                _ ->
                    %% otherwise, update target
                    NewHatreds = lists:sublist([#hatred{id = Id, type = ?MAP_OBJECT_ROLE} | lists:keydelete(Id, #hatred.id, Hatreds)], 3),
                    NewFighterList = lists:keyreplace(TargetId, #fighter.id, FighterList, FinalTarget#fighter{hatreds = NewHatreds})
            end,
            %% update hurt rank
            battle_rank:update(FinalState, FinalAttacker, FinalHurt, Now, hurt),
            %% handle battle event
            HandleEventState = handle_battle_event(FinalState#map_state{fighter = NewFighterList}, FinalAttacker, FinalTarget, FinalHurt),
            %% continue
            perform_skill_loop(HandleEventState, FinalAttacker, Skill, TargetList, Now, Hurt + FinalHurt, [FinalTarget | List]);
        _ ->
            perform_skill_loop(State, Attacker, Skill, TargetList, Now, Hurt, List)
    end.

%% find and check target attribute
check_target(State = #map_state{fighter = FighterList}, Attacker = #fighter{id = Id, camp = Camp}, TargetId, Distance) ->
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
            {error, same_cap};
        Target = #fighter{} ->
            check_target_state(State, Attacker, Target, Distance)
    end.

check_target_state(State, Attacker, Target, Distance) ->
    %% check attacker can be-attack or not, and check they distance
    case not battle_attribute:check(Target, cannot_be_attack) of
        true ->
            check_target_distance(State, Attacker, Target, Distance);
        false ->
            {error, cannot_be_attack}
    end.

check_target_distance(_State, Attacker, Target, Distance) ->
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
