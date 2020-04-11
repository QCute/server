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
    case validate_attacker(State, AttackerId, SkillId, Now) of
        {ok, Attacker, Skill} ->
            case validate_target_number(State, Attacker, Skill, TargetList) of
                ok ->
                    perform_skill(State, Attacker, Skill, TargetList, Now);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% validate attacker
validate_attacker(State = #map_state{fighters = Fighters}, AttackerId, SkillId, Now) ->
    case lists:keyfind(AttackerId, #fighter.id, Fighters) of
        Attacker = #fighter{attribute = #attribute{}} ->
            %% check attacker can attack or not
            case battle_attribute:check(Attacker, cannot_attack) of
                false ->
                    validate_skill(State, Attacker, SkillId, Now);
                true ->
                    {error, cannot_attack}
            end;
        false ->
            {error, no_such_attacker}
    end.

%% validate attacker battle skill
validate_skill(_State, Attacker = #fighter{skills = Skills}, SkillId, Now) ->
    case lists:keyfind(SkillId, #battle_skill.skill_id, Skills) of
        Skill = #battle_skill{cd = Cd} when Cd =< Now ->
            {ok, Attacker, Skill};
        #battle_skill{} ->
            {error, skill_cd};
        false ->
            {error, no_such_skill}
    end.

%% check skill effect number great then or equals target number
validate_target_number(_State, _Attacker, #battle_skill{number = Number}, TargetList) ->
    case length(TargetList) =< Number of
        true ->
            ok;
        false ->
            {error, too_many_target}
    end.

%% perform skill
perform_skill(State, Attacker = #fighter{id = Id, sender_pid = SenderPid, skills = Skills, x = X, y = Y}, Skill = #battle_skill{skill_id = SkillId}, TargetList, Now) ->
    {NewState = #map_state{fighters = Fighters}, NewAttacker, _, List} = perform_skill_loop(State, Attacker, Skill, TargetList, Now, 0, []),
    %% update skill cd
    NewSkills = lists:keyreplace(SkillId, #battle_skill.skill_id, Skills, Skill#battle_skill{time = Now}),
    FinalAttacker = NewAttacker#fighter{skills = NewSkills},
    %% update self data
    user_sender:send(SenderPid, ?PROTOCOL_MAP_SELF, FinalAttacker),
    %% update attacker
    NewFighters = lists:keyreplace(Id, #fighter.id, Fighters, FinalAttacker),
    %% notify target data to client
    {ok, Binary} = user_router:write(?PROTOCOL_MAP_FIGHTER, List),
    map:notify(NewState, X, Y, Binary),
    %% return new state
    {ok, NewState#map_state{fighters = NewFighters}}.

%% perform skill for each one target
perform_skill_loop(State, Attacker, _, [], _, Hurt, List) ->
    {State, Attacker, Hurt, List};
perform_skill_loop(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id}, Skill = #battle_skill{distance = Distance}, [TargetId | TargetList], Now, Hurt, List) ->
    case validate_target(State, Attacker, TargetId, Distance) of
        {ok, Target= #fighter{hatreds = Hatreds}} ->
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
                    NewFighters = lists:keydelete(TargetId, #fighter.id, Fighters);
                _ ->
                    %% otherwise, update target
                    NewHatreds = lists:sublist(lists:keystore(Id, #hatred.id, Hatreds, #hatred{id = Id, type = ?MAP_OBJECT_ROLE}), 3),
                    NewFighters = lists:keyreplace(TargetId, #fighter.id, Fighters, FinalTarget#fighter{hatreds = NewHatreds})
            end,
            %% update hurt rank
            battle_rank:update(NewState, FinalAttacker, FinalHurt, Now, hurt),
            %% handle battle event
            HandleEventState = handle_battle_event(FinalState#map_state{fighters = NewFighters}, FinalAttacker, FinalTarget, FinalHurt),
            %% continue
            perform_skill_loop(HandleEventState, FinalAttacker, Skill, TargetList, Now, Hurt + FinalHurt, [FinalTarget | List]);
        _ ->
            perform_skill_loop(State, Attacker, Skill, TargetList, Now, Hurt, List)
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
