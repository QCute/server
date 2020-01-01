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
-include("map.hrl").
-include("attribute.hrl").
-include("skill.hrl").
-include("protocol.hrl").
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
    case perform_skill_loop(State, Attacker, Skill, TargetList, []) of
        {NewState = #map_state{fighters = Fighters}, NewAttacker, List} ->
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
perform_skill_loop(State, Attacker, _, [], List) ->
    {State, Attacker, List};
perform_skill_loop(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id, camp = Camp}, Skill = #battle_skill{distance = Distance}, [#hatred{id = TargetId} | TargetList], List) ->
    case lists:keyfind(TargetId, #fighter.id, Fighters) of
        false ->
            %% no such target
            {error, no_such_target};
        #fighter{attribute = #attribute{hp = 0}} ->
            %% filter dead object
            {error, dead_object};
        #fighter{camp = Camp} ->
            %% same camp
            {error, same_cap};
        Target = #fighter{hatreds = Hatreds} ->
            %% check attacker can be-attack or not, and check they distance
            case not battle_attribute:check(Target, cannot_be_attack) of
                true ->
                    case map:is_in_distance(Attacker, Target, Distance) of
                        true ->
                            %% base attribute hurt
                            Hurt = battle_attribute:calculate_hurt(Attacker, Target),
                            %% perform skill, execute skill effect
                            {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, Hurt),
                            %% perform passive skill, execute skill effect
                            {FinalState, FinalAttacker, FinalTarget, _FinalHurt} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
                            %% update target
                            NewHatreds = lists:sublist([#hatred{id = Id, type = ?MAP_OBJECT_ROLE} | Hatreds], 3),
                            NewFighters = lists:keyreplace(TargetId, #fighter.id, Fighters, FinalTarget#fighter{hatreds = NewHatreds}),
                            %% continue
                            perform_skill_loop(FinalState#map_state{fighters = NewFighters}, FinalAttacker, Skill, TargetList, [FinalTarget | List]);
                        false ->
                            {error, distance_far_away}
                    end;
                false ->
                    {error, cannot_be_attack}
            end
    end;
perform_skill_loop(State, Attacker, Skill, [_ | TargetList], List) ->
    %% health point less then or equal zero state
    perform_skill_loop(State, Attacker, Skill, TargetList, List).

